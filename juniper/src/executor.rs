use std::cmp::Ordering;
use std::fmt::Display;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use fnv::FnvHashMap;
use futures::{future, Future};

use GraphQLError;
use ast::{Definition, Document, Fragment, FromInputValue, InputValue, OperationType, Selection,
          ToInputValue, Type};
use value::Value;
use parser::SourcePosition;
use shared_str::SharedStr;

use schema::meta::{Argument, EnumMeta, EnumValue, Field, InputObjectMeta, InterfaceMeta, ListMeta,
                   MetaType, NullableMeta, ObjectMeta, PlaceholderMeta, ScalarMeta, UnionMeta};
use schema::model::{RootNode, SchemaType};

use types::base::GraphQLType;
use types::name::Name;

/// A type registry used to build schemas
///
/// The registry gathers metadata for all types in a schema. It provides
/// convenience methods to convert types implementing the `GraphQLType` trait
/// into `Type` instances and automatically registers them.
pub struct Registry {
    /// Currently registered types
    pub types: FnvHashMap<Name, MetaType>,
}

#[derive(Clone)]
pub enum FieldPath {
    Root(SourcePosition),
    Field(SharedStr, SourcePosition, Arc<FieldPath>),
}

/// Query execution engine
///
/// The executor helps drive the query execution in a schema. It keeps track
/// of the current field stack, context, variables, and errors.
pub struct Executor<CtxT>
where
    CtxT: 'static,
{
    fragments: Arc<HashMap<SharedStr, Arc<Fragment>>>,
    variables: Arc<Variables>,
    current_selection_set: Option<Arc<Vec<Selection>>>,
    schema: Arc<SchemaType>,
    context: Arc<CtxT>,
    errors: Arc<RwLock<Vec<ExecutionError>>>,
    field_path: Arc<FieldPath>,
}

/// Error type for errors that occur during query execution
///
/// All execution errors contain the source position in the query of the field
/// that failed to resolve. It also contains the field stack.
#[derive(Debug, PartialEq)]
pub struct ExecutionError {
    location: SourcePosition,
    path: Vec<String>,
    error: FieldError,
}

impl Eq for ExecutionError {}

impl PartialOrd for ExecutionError {
    fn partial_cmp(&self, other: &ExecutionError) -> Option<Ordering> {
        (&self.location, &self.path, &self.error.message)
            .partial_cmp(&(&other.location, &other.path, &other.error.message))
    }
}

impl Ord for ExecutionError {
    fn cmp(&self, other: &ExecutionError) -> Ordering {
        (&self.location, &self.path, &self.error.message)
            .cmp(&(&other.location, &other.path, &other.error.message))
    }
}

/// Error type for errors that occur during field resolution
///
/// Field errors are represented by a human-readable error message and an
/// optional `Value` structure containing additional information.
///
/// They can be converted to from any type that implements `std::fmt::Display`,
/// which makes error chaining with the `?` operator a breeze:
///
/// ```rust
/// # use juniper::FieldError;
/// fn get_string(data: Vec<u8>) -> Result<String, FieldError> {
///     let s = String::from_utf8(data)?;
///     Ok(s)
/// }
/// ```
#[derive(Debug, PartialEq)]
pub struct FieldError {
    message: String,
    data: Value,
}

impl<T: Display> From<T> for FieldError {
    fn from(e: T) -> FieldError {
        FieldError {
            message: format!("{}", e),
            data: Value::null(),
        }
    }
}

impl FieldError {
    /// Construct a new error with additional data
    ///
    /// You can use the `graphql_value!` macro to construct an error:
    ///
    /// ```rust
    /// # #[macro_use] extern crate juniper;
    /// use juniper::FieldError;
    ///
    /// # fn sample() {
    /// FieldError::new(
    ///     "Could not open connection to the database",
    ///     graphql_value!({ "internal_error": "Connection refused" })
    /// );
    /// # }
    /// # fn main() { }
    /// ```
    ///
    /// The `data` parameter will be added to the `"data"` field of the error
    /// object in the JSON response:
    ///
    /// ```json
    /// {
    ///   "errors": [
    ///     "message": "Could not open connection to the database",
    ///     "locations": [{"line": 2, "column": 4}],
    ///     "data": {
    ///       "internal_error": "Connection refused"
    ///     }
    ///   ]
    /// }
    /// ```
    ///
    /// If the argument is `Value::null()`, no extra data will be included.
    pub fn new<T: Display>(e: T, data: Value) -> FieldError {
        FieldError {
            message: format!("{}", e),
            data: data,
        }
    }

    #[doc(hidden)]
    pub fn message(&self) -> &str {
        &self.message
    }

    #[doc(hidden)]
    pub fn data(&self) -> &Value {
        &self.data
    }
}

/// The result of resolving the value of a field of type `T`
pub type FieldResult<T> = Result<T, FieldError>;

/// The result of resolving an unspecified field
pub type ExecutionResult = DelayedResult<Value, FieldError>;

/// The map of variables used for substitution during query execution
pub type Variables = HashMap<String, InputValue>;

pub enum SyncResult<T, E> {
    Ok(T),
    Err(E),
}

impl<T, E> SyncResult<T, E> {
    fn from_result(result: Result<T, E>) -> SyncResult<T, E> {
        match result {
            Ok(v) => SyncResult::Ok(v),
            Err(e) => SyncResult::Err(e),
        }
    }
}

impl<T: GraphQLType<Context=NewCtxT>, NewCtxT: 'static> SyncResult<Option<(Arc<NewCtxT>, T)>, FieldError> {
    pub fn replace_context_and_resolve<CtxT>(self, info: &T::TypeInfo, executor: &Arc<Executor<CtxT>>) -> ExecutionResult 
        where T::Context: FromContext<NewCtxT>
    {
        match self {
            SyncResult::Ok(Some((ctx, r))) => 
                executor.replaced_context(ctx).resolve_with_ctx(info, &r),
            SyncResult::Ok(None) =>
                DelayedResult::sync_ok(Value::null()),
            SyncResult::Err(e) =>
                DelayedResult::sync_err(e),
        }
    }
}

pub enum DelayedResult<T, E> {
    Sync(SyncResult<T, E>),
    Async(Box<Future<Item=T, Error=E>>),
}

impl<T, E> DelayedResult<T, E> {
    pub fn sync_ok(v: T) -> DelayedResult<T, E> {
        DelayedResult::Sync(SyncResult::Ok(v))
    }

    pub fn sync_err(e: E) -> DelayedResult<T, E> {
        DelayedResult::Sync(SyncResult::Err(e))
    }
}

impl<T, E> From<SyncResult<T, E>> for DelayedResult<T, E> {
    fn from(v: SyncResult<T, E>) -> DelayedResult<T, E> {
        DelayedResult::Sync(v)
    }
}

type DelayedFieldResult<T> = DelayedResult<T, FieldError>;


#[doc(hidden)]
pub trait IntoResolvable<T: GraphQLType, C>: Sized {
    type R: Into<DelayedFieldResult<Option<(Arc<T::Context>, T)>>>;

    #[doc(hidden)]
    fn into(self, ctx: Arc<C>) -> Self::R;
}

impl<T: GraphQLType, C> IntoResolvable<T, C> for T
where
    T::Context: FromContext<C>,
{
    type R = SyncResult<Option<(Arc<T::Context>, T)>, FieldError>;

    fn into(self, ctx: Arc<C>) -> Self::R {
        SyncResult::Ok(Some((FromContext::from(ctx), self)))
    }
}

impl<T: GraphQLType, C> IntoResolvable<T, C> for FieldResult<T>
where
    T::Context: FromContext<C>,
{
    type R = SyncResult<Option<(Arc<T::Context>, T)>, FieldError>;

    fn into(self, ctx: Arc<C>) -> Self::R {
        SyncResult::from_result(self.map(|v| Some((FromContext::from(ctx), v))))
    }
}

impl<T: GraphQLType, C> IntoResolvable<T, C> for (Arc<T::Context>, T) {
    type R = SyncResult<Option<(Arc<T::Context>, T)>, FieldError>;

    fn into(self, _: Arc<C>) -> Self::R {
        SyncResult::Ok(Some(self))
    }
}

impl<T: GraphQLType, C> IntoResolvable<Option<T>, C> for Option<(Arc<T::Context>, T)> {
    type R = SyncResult<Option<(Arc<T::Context>, Option<T>)>, FieldError>;

    fn into(self, _: Arc<C>) -> Self::R {
        SyncResult::Ok(self.map(|(ctx, v)| (ctx, Some(v))))
    }
}

impl<T: GraphQLType, C> IntoResolvable<T, C> for FieldResult<(Arc<T::Context>, T)> {
    type R = SyncResult<Option<(Arc<T::Context>, T)>, FieldError>;

    fn into(self, _: Arc<C>) -> Self::R {
        SyncResult::from_result(self.map(Some))
    }
}

impl<T: GraphQLType, C> IntoResolvable<Option<T>, C> for FieldResult<Option<(Arc<T::Context>, T)>> {
    type R = SyncResult<Option<(Arc<T::Context>, Option<T>)>, FieldError>;

    fn into(self, _: Arc<C>) -> Self::R {
        SyncResult::from_result(self.map(|o| o.map(|(ctx, v)| (ctx, Some(v)))))
    }
}

/// Conversion trait for context types
///
/// Used to support different context types for different parts of an
/// application. By making each GraphQL type only aware of as much
/// context as it needs to, isolation and robustness can be
/// improved. Implement this trait if you have contexts that can
/// generally be converted between each other.
///
/// The empty tuple `()` can be converted into from any context type,
/// making it suitable for GraphQL that don't need _any_ context to
/// work, e.g. scalars or enums.
pub trait FromContext<T> {
    /// Perform the conversion
    fn from(value: Arc<T>) -> Arc<Self>;
}

/// Marker trait for types that can act as context objects for GraphQL types.
pub trait Context {}

impl<'a, C: Context> Context for &'a C {}

impl<T> FromContext<T> for () {
    fn from(_: Arc<T>) -> Arc<Self> {
        Arc::new(())
    }
}

impl<T> FromContext<T> for T
where
    T: Context,
{
    fn from(value: Arc<T>) -> Arc<Self> {
        value
    }
}

pub fn resolve_maybe_delayed_iterator<I: Iterator<Item=ExecutionResult>>(iter: I) -> ExecutionResult {
    let mut values = vec![];

    for res in iter {
        match res {
            DelayedResult::Sync(SyncResult::Ok(v)) => values.push(v),
            DelayedResult::Sync(SyncResult::Err(_)) => values.push(Value::null()),
            DelayedResult::Async(_) => unimplemented!(), // MH: FIXME ASYNC
        }
    }
    
    DelayedResult::sync_ok(Value::list(values))
}

impl<CtxT> Executor<CtxT> {
    /// Resolve a single arbitrary value, mapping the context to a new type
    pub fn resolve_with_ctx<NewCtxT: 'static, T: GraphQLType<Context = NewCtxT>>(
        &self,
        info: &T::TypeInfo,
        value: &T,
    ) -> ExecutionResult
    where
        NewCtxT: FromContext<CtxT>,
    {
        Executor::resolve(
            self.replaced_context(<NewCtxT as FromContext<CtxT>>::from(self.context.clone())),
            info, value
        )
    }

    /// Resolve a single arbitrary value into an `ExecutionResult`
    pub fn resolve<T: GraphQLType<Context = CtxT>>(
        this: Arc<Self>,
        info: &T::TypeInfo,
        value: &T,
    ) -> ExecutionResult {
        value.resolve(info, this.current_selection_set.as_ref(), this.clone())
    }

    /// Resolve a single arbitrary value into a return value
    ///
    /// If the field fails to resolve, `null` will be returned.
    pub fn resolve_into_value<T: GraphQLType<Context = CtxT>>(
        this: Arc<Self>,
        info: &T::TypeInfo,
        value: &T,
    ) -> ExecutionResult {
        
        match Executor::resolve(this.clone(), info, value) {
            DelayedResult::Sync(SyncResult::Ok(v)) => DelayedResult::sync_ok(v),
            DelayedResult::Sync(SyncResult::Err(e)) => {
                this.push_error(e);
                DelayedResult::sync_ok(Value::null())
            },
            DelayedResult::Async(_) => unimplemented!() // MH: FIXME ASYNC
        }
    }

    /// Derive a new executor by replacing the context
    ///
    /// This can be used to connect different types, e.g. from different Rust
    /// libraries, that require different context types.
    pub fn replaced_context<NewCtxT: 'static>(&self, ctx: Arc<NewCtxT>) -> Arc<Executor<NewCtxT>> {
        Arc::new(Executor {
            fragments: self.fragments.clone(),
            variables: self.variables.clone(),
            current_selection_set: self.current_selection_set.clone(),
            schema: self.schema.clone(),
            context: ctx,
            errors: self.errors.clone(),
            field_path: self.field_path.clone(),
        })
    }

    #[doc(hidden)]
    pub fn sub_executor(
        &self,
        field_name: Option<SharedStr>,
        location: SourcePosition,
        selection_set: Option<Arc<Vec<Selection>>>,
    ) -> Arc<Executor<CtxT>> {
        Arc::new(Executor {
            fragments: self.fragments.clone(),
            variables: self.variables.clone(),
            current_selection_set: selection_set,
            schema: self.schema.clone(),
            context: self.context.clone(),
            errors: self.errors.clone(),
            field_path: match field_name {
                Some(name) => Arc::new(FieldPath::Field(name, location, self.field_path.clone())),
                None => self.field_path.clone(),
            },
        })
    }

    /// Access the current context
    ///
    /// You usually provide the context when calling the top-level `execute`
    /// function, or using the context factory in the Iron integration.
    pub fn context(&self) -> &CtxT {
        &self.context
    }

    pub fn arc_context(&self) -> Arc<CtxT> {
        self.context.clone()
    }

    /// The currently executing schema
    pub fn schema(&self) -> &SchemaType {
        &self.schema
    }

    #[doc(hidden)]
    pub fn variables(&self) -> &Variables {
        &self.variables
    }

    #[doc(hidden)]
    pub fn fragment_by_name(&self, name: &str) -> Option<&Fragment> {
        self.fragments.get(name).map(|f| f.as_ref())
    }

    /// The current location of the executor
    pub fn location(&self) -> &SourcePosition {
        self.field_path.location()
    }

    /// Add an error to the execution engine at the current executor location
    pub fn push_error(&self, error: FieldError) {
        let location = self.location().clone();
        self.push_error_at(error, location);
    }

    /// Add an error to the execution engine at a specific location
    pub fn push_error_at(&self, error: FieldError, location: SourcePosition) {
        let mut path = Vec::new();
        self.field_path.construct_path(&mut path);

        let mut errors = self.errors.write().unwrap();

        errors.push(ExecutionError {
            location: location,
            path: path,
            error: error,
        });
    }
}

impl FieldPath {
    fn construct_path(&self, acc: &mut Vec<String>) {
        match *self {
            FieldPath::Root(_) => (),
            FieldPath::Field(ref name, _, ref parent) => {
                parent.construct_path(acc);
                acc.push(name.to_string());
            }
        }
    }

    fn location(&self) -> &SourcePosition {
        match *self {
            FieldPath::Root(ref pos) | FieldPath::Field(_, ref pos, _) => pos,
        }
    }
}

impl ExecutionError {
    #[doc(hidden)]
    pub fn new(location: SourcePosition, path: &[&str], error: FieldError) -> ExecutionError {
        ExecutionError {
            location: location,
            path: path.iter().map(|s| (*s).to_owned()).collect(),
            error: error,
        }
    }

    /// The error message
    pub fn error(&self) -> &FieldError {
        &self.error
    }

    /// The source location _in the query_ of the field that failed to resolve
    pub fn location(&self) -> &SourcePosition {
        &self.location
    }

    /// The path of fields leading to the field that generated this error
    pub fn path(&self) -> &[String] {
        &self.path
    }
}

pub fn execute_validated_query<QueryT, MutationT, CtxT: 'static>(
    document: Document,
    operation_name: Option<&str>,
    root_node: &RootNode<QueryT, MutationT>,
    variables: &Variables,
    context: Arc<CtxT>,
) -> Box<Future<Item=(Value, Vec<ExecutionError>), Error=GraphQLError>>
where
    QueryT: GraphQLType<Context = CtxT>,
    MutationT: GraphQLType<Context = CtxT>,
{
    let mut fragments = vec![];
    let mut operation = None;

    for def in document {
        match def {
            Definition::Operation(op) => {
                if operation_name.is_none() && operation.is_some() {
                    return Box::new(future::err(GraphQLError::MultipleOperationsProvided));
                }

                let move_op = operation_name.is_none() ||
                    op.item.name.as_ref().map(|s| s.item.as_str()) == operation_name;

                if move_op {
                    operation = Some(op);
                }
            }
            Definition::Fragment(f) => fragments.push(f),
        };
    }

    let op = match operation {
        Some(op) => op,
        None => return Box::new(future::err(GraphQLError::UnknownOperationName)),
    };

    let default_variable_values = op.item.variable_definitions.map(|defs| {
        defs.item
            .items
            .iter()
            .filter_map(|&(ref name, ref def)| {
                def.default_value
                    .as_ref()
                    .map(|i| (name.item.to_string(), i.item.clone()))
            })
            .collect::<HashMap<String, InputValue>>()
    });

    let errors = Arc::new(RwLock::new(Vec::new()));
    let value;

    {
        let mut all_vars;
        let mut final_vars = variables;

        if let Some(defaults) = default_variable_values {
            all_vars = variables.clone();

            for (name, value) in defaults {
                all_vars.entry(name).or_insert(value);
            }

            final_vars = &all_vars;
        }

        let executor = Arc::new(Executor {
            fragments: Arc::new(fragments
                .iter()
                .map(|f| (f.item.name.item.clone(), Arc::new(f.item.clone())))
                .collect()),
            variables: Arc::new(final_vars.clone()),
            current_selection_set: Some(Arc::new(op.item.selection_set[..].to_vec())),
            schema: root_node.schema.clone(),
            context: context,
            errors: errors.clone(),
            field_path: Arc::new(FieldPath::Root(op.start)),
        });

        value = match op.item.operation_type {
            OperationType::Query => Executor::resolve_into_value(executor, &root_node.query_info, &root_node),
            OperationType::Mutation => {
                Executor::resolve_into_value(executor, &root_node.mutation_info, &root_node.mutation_type)
            }
        };
    }

    let mut errors = Arc::try_unwrap(errors).unwrap().into_inner().unwrap();
    errors.sort();

    match value {
        DelayedResult::Sync(SyncResult::Ok(v)) => Box::new(future::ok((v, errors))),
        DelayedResult::Sync(SyncResult::Err(_)) => Box::new(future::ok((Value::null(), errors))),
        DelayedResult::Async(_) => unimplemented!(), // MH: FIXME ASYNC
    }
}

impl Registry {
    /// Construct a new registry
    pub fn new(types: FnvHashMap<Name, MetaType>) -> Registry {
        Registry { types: types }
    }

    /// Get the `Type` instance for a given GraphQL type
    ///
    /// If the registry hasn't seen a type with this name before, it will
    /// construct its metadata and store it.
    pub fn get_type<T>(&mut self, info: &T::TypeInfo) -> Type
    where
        T: GraphQLType,
    {
        if let Some(name) = T::name(info) {
            let validated_name = name.parse::<Name>().unwrap();
            if !self.types.contains_key(name) {
                self.insert_placeholder(
                    validated_name.clone(),
                    Type::NonNullNamed(SharedStr::clone_from_str(name)),
                );
                let meta = T::meta(info, self);
                self.types.insert(validated_name, meta);
            }
            self.types[name].as_type()
        } else {
            T::meta(info, self).as_type()
        }
    }

    /// Create a field with the provided name
    pub fn field<T>(&mut self, name: &str, info: &T::TypeInfo) -> Field
    where
        T: GraphQLType,
    {
        Field {
            name: name.to_owned(),
            description: None,
            arguments: None,
            field_type: self.get_type::<T>(info),
            deprecation_reason: None,
        }
    }

    #[doc(hidden)]
    pub fn field_convert<T: IntoResolvable<I, C>, I, C>(
        &mut self,
        name: &str,
        info: &I::TypeInfo,
    ) -> Field
    where
        I: GraphQLType,
    {
        Field {
            name: name.to_owned(),
            description: None,
            arguments: None,
            field_type: self.get_type::<I>(info),
            deprecation_reason: None,
        }
    }

    /// Create an argument with the provided name
    pub fn arg<T>(&mut self, name: &str, info: &T::TypeInfo) -> Argument
    where
        T: GraphQLType + FromInputValue,
    {
        Argument::new(name, self.get_type::<T>(info))
    }

    /// Create an argument with a default value
    ///
    /// When called with type `T`, the actual argument will be given the type
    /// `Option<T>`.
    pub fn arg_with_default<T>(&mut self, name: &str, value: &T, info: &T::TypeInfo) -> Argument
    where
        T: GraphQLType + ToInputValue + FromInputValue,
    {
        Argument::new(name, self.get_type::<Option<T>>(info)).default_value(value.to_input_value())
    }

    fn insert_placeholder(&mut self, name: Name, of_type: Type) {
        if !self.types.contains_key(&name) {
            self.types.insert(
                name,
                MetaType::Placeholder(PlaceholderMeta { of_type: of_type }),
            );
        }
    }

    /// Create a scalar meta type
    ///
    /// This expects the type to implement `FromInputValue`.
    pub fn build_scalar_type<T>(&mut self, info: &T::TypeInfo) -> ScalarMeta
    where
        T: FromInputValue + GraphQLType,
    {
        let name = T::name(info).expect("Scalar types must be named. Implement name()");
        ScalarMeta::new::<T>(name.to_string())
    }

    /// Create a list meta type
    pub fn build_list_type<T: GraphQLType>(&mut self, info: &T::TypeInfo) -> ListMeta {
        let of_type = self.get_type::<T>(info);
        ListMeta::new(of_type)
    }

    /// Create a nullable meta type
    pub fn build_nullable_type<T: GraphQLType>(&mut self, info: &T::TypeInfo) -> NullableMeta {
        let of_type = self.get_type::<T>(info);
        NullableMeta::new(of_type)
    }

    /// Create an object meta type builder
    ///
    /// To prevent infinite recursion by enforcing ordering, this returns a
    /// function that needs to be called with the list of fields on the object.
    pub fn build_object_type<T>(
        &mut self,
        info: &T::TypeInfo,
        fields: &[Field],
    ) -> ObjectMeta
    where
        T: GraphQLType,
    {
        let name = T::name(info).expect("Object types must be named. Implement name()");

        let mut v = fields.to_vec();
        v.push(self.field::<String>("__typename", &()));
        ObjectMeta::new(name.to_string(), &v)
    }

    /// Create an enum meta type
    pub fn build_enum_type<T>(&mut self, info: &T::TypeInfo, values: &[EnumValue]) -> EnumMeta
    where
        T: FromInputValue + GraphQLType,
    {
        let name = T::name(info).expect("Enum types must be named. Implement name()");

        EnumMeta::new::<T>(name.to_string(), values)
    }

    /// Create an interface meta type builder,
    /// by providing a type info object.
    pub fn build_interface_type<T>(
        &mut self,
        info: &T::TypeInfo,
        fields: &[Field],
    ) -> InterfaceMeta
    where
        T: GraphQLType,
    {
        let name = T::name(info).expect("Interface types must be named. Implement name()");

        let mut v = fields.to_vec();
        v.push(self.field::<String>("__typename", &()));
        InterfaceMeta::new(name.to_string(), &v)
    }

    /// Create a union meta type builder
    pub fn build_union_type<T>(&mut self, info: &T::TypeInfo, types: &[Type]) -> UnionMeta
    where
        T: GraphQLType,
    {
        let name = T::name(info).expect("Union types must be named. Implement name()");

        UnionMeta::new(name.to_string(), types)
    }

    /// Create an input object meta type builder
    pub fn build_input_object_type<T>(
        &mut self,
        info: &T::TypeInfo,
        args: &[Argument],
    ) -> InputObjectMeta
    where
        T: FromInputValue + GraphQLType,
    {
        let name = T::name(info).expect("Input object types must be named. Implement name()");

        InputObjectMeta::new::<T>(name.to_string(), args)
    }
}
