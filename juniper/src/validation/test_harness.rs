use ast::{FromInputValue, InputValue};
use types::base::GraphQLType;
use executor::Registry;
use types::scalars::ID;
use schema::meta::{EnumValue, MetaType};
use validation::RuleError;

struct Being;
struct Pet;
struct Canine;

struct Dog;
struct Cat;

struct Intelligent;
struct Human;
struct Alien;

struct DogOrHuman;
struct CatOrDog;
struct HumanOrAlien;

struct ComplicatedArgs;

pub struct QueryRoot;

#[derive(Debug)]
enum DogCommand {
    Sit,
    Heel,
    Down,
}

#[derive(Debug)]
enum FurColor {
    Brown,
    Black,
    Tan,
    Spotted,
}

#[allow(dead_code)]
#[derive(Debug)]
struct ComplexInput {
    required_field: bool,
    int_field: Option<i32>,
    string_field: Option<String>,
    boolean_field: Option<bool>,
    string_list_field: Option<Vec<Option<String>>>,
}

impl GraphQLType for Being {
    type Context = ();
    type TypeInfo = ();

    fn name(_: &()) -> Option<&'static str> {
        Some("Being")
    }

    fn meta(i: &(), registry: &mut Registry) -> MetaType {
        let fields = &[
            registry
                .field::<Option<String>>("name", i)
                .argument(registry.arg::<Option<bool>>("surname", i)),
        ];

        registry.build_interface_type::<Self>(i, fields).into_meta()
    }
}

impl GraphQLType for Pet {
    type Context = ();
    type TypeInfo = ();

    fn name(_: &()) -> Option<&'static str> {
        Some("Pet")
    }

    fn meta(i: &(), registry: &mut Registry) -> MetaType {
        let fields = &[
            registry
                .field::<Option<String>>("name", i)
                .argument(registry.arg::<Option<bool>>("surname", i)),
        ];

        registry.build_interface_type::<Self>(i, fields).into_meta()
    }
}

impl GraphQLType for Canine {
    type Context = ();
    type TypeInfo = ();

    fn name(_: &()) -> Option<&'static str> {
        Some("Canine")
    }

    fn meta(i: &(), registry: &mut Registry) -> MetaType {
        let fields = &[
            registry
                .field::<Option<String>>("name", i)
                .argument(registry.arg::<Option<bool>>("surname", i)),
        ];

        registry.build_interface_type::<Self>(i, fields).into_meta()
    }
}

impl GraphQLType for DogCommand {
    type Context = ();
    type TypeInfo = ();

    fn name(_: &()) -> Option<&'static str> {
        Some("DogCommand")
    }

    fn meta(i: &(), registry: &mut Registry) -> MetaType {
        registry
            .build_enum_type::<Self>(
                i,
                &[
                    EnumValue::new("SIT"),
                    EnumValue::new("HEEL"),
                    EnumValue::new("DOWN"),
                ],
            )
            .into_meta()
    }
}

impl FromInputValue for DogCommand {
    fn from_input_value(v: &InputValue) -> Option<DogCommand> {
        match v.as_enum_value() {
            Some("SIT") => Some(DogCommand::Sit),
            Some("HEEL") => Some(DogCommand::Heel),
            Some("DOWN") => Some(DogCommand::Down),
            _ => None,
        }
    }
}

impl GraphQLType for Dog {
    type Context = ();
    type TypeInfo = ();

    fn name(_: &()) -> Option<&'static str> {
        Some("Dog")
    }

    fn meta(i: &(), registry: &mut Registry) -> MetaType {
        let fields = &[
            registry
                .field::<Option<String>>("name", i)
                .argument(registry.arg::<Option<bool>>("surname", i)),
            registry.field::<Option<String>>("nickname", i),
            registry.field::<Option<i32>>("barkVolume", i),
            registry.field::<Option<bool>>("barks", i),
            registry
                .field::<Option<bool>>("doesKnowCommand", i)
                .argument(registry.arg::<Option<DogCommand>>("dogCommand", i)),
            registry
                .field::<Option<bool>>("isHousetrained", i)
                .argument(registry.arg_with_default("atOtherHomes", &true, i)),
            registry
                .field::<Option<bool>>("isAtLocation", i)
                .argument(registry.arg::<Option<i32>>("x", i))
                .argument(registry.arg::<Option<i32>>("y", i)),
        ];

        registry
            .build_object_type::<Self>(i, fields)
            .interfaces(&[
                registry.get_type::<Being>(i),
                registry.get_type::<Pet>(i),
                registry.get_type::<Canine>(i),
            ])
            .into_meta()
    }
}

impl GraphQLType for FurColor {
    type Context = ();
    type TypeInfo = ();

    fn name(_: &()) -> Option<&'static str> {
        Some("FurColor")
    }

    fn meta(i: &(), registry: &mut Registry) -> MetaType {
        registry
            .build_enum_type::<Self>(
                i,
                &[
                    EnumValue::new("BROWN"),
                    EnumValue::new("BLACK"),
                    EnumValue::new("TAN"),
                    EnumValue::new("SPOTTED"),
                ],
            )
            .into_meta()
    }
}

impl FromInputValue for FurColor {
    fn from_input_value(v: &InputValue) -> Option<FurColor> {
        match v.as_enum_value() {
            Some("BROWN") => Some(FurColor::Brown),
            Some("BLACK") => Some(FurColor::Black),
            Some("TAN") => Some(FurColor::Tan),
            Some("SPOTTED") => Some(FurColor::Spotted),
            _ => None,
        }
    }
}

impl GraphQLType for Cat {
    type Context = ();
    type TypeInfo = ();

    fn name(_: &()) -> Option<&'static str> {
        Some("Cat")
    }

    fn meta(i: &(), registry: &mut Registry) -> MetaType {
        let fields = &[
            registry
                .field::<Option<String>>("name", i)
                .argument(registry.arg::<Option<bool>>("surname", i)),
            registry.field::<Option<String>>("nickname", i),
            registry.field::<Option<bool>>("meows", i),
            registry.field::<Option<i32>>("meowVolume", i),
            registry.field::<Option<FurColor>>("furColor", i),
        ];

        registry
            .build_object_type::<Self>(i, fields)
            .interfaces(&[
                registry.get_type::<Being>(i),
                registry.get_type::<Pet>(i),
            ])
            .into_meta()
    }
}

impl GraphQLType for CatOrDog {
    type Context = ();
    type TypeInfo = ();

    fn name(_: &()) -> Option<&'static str> {
        Some("CatOrDog")
    }

    fn meta(i: &(), registry: &mut Registry) -> MetaType {
        let types = &[registry.get_type::<Cat>(i), registry.get_type::<Dog>(i)];

        registry.build_union_type::<Self>(i, types).into_meta()
    }
}

impl GraphQLType for Intelligent {
    type Context = ();
    type TypeInfo = ();

    fn name(_: &()) -> Option<&'static str> {
        Some("Intelligent")
    }

    fn meta(i: &(), registry: &mut Registry) -> MetaType {
        let fields = &[registry.field::<Option<i32>>("iq", i)];

        registry.build_interface_type::<Self>(i, fields).into_meta()
    }
}

impl GraphQLType for Human {
    type Context = ();
    type TypeInfo = ();

    fn name(_: &()) -> Option<&'static str> {
        Some("Human")
    }

    fn meta(i: &(), registry: &mut Registry) -> MetaType {
        let fields = &[
            registry
                .field::<Option<String>>("name", i)
                .argument(registry.arg::<Option<bool>>("surname", i)),
            registry.field::<Option<Vec<Option<Pet>>>>("pets", i),
            registry.field::<Option<Vec<Human>>>("relatives", i),
            registry.field::<Option<i32>>("iq", i),
        ];
        registry
            .build_object_type::<Self>(i, fields)
            .interfaces(&[
                registry.get_type::<Being>(i),
                registry.get_type::<Intelligent>(i),
            ])
            .into_meta()
    }
}

impl GraphQLType for Alien {
    type Context = ();
    type TypeInfo = ();

    fn name(_: &()) -> Option<&'static str> {
        Some("Alien")
    }

    fn meta(i: &(), registry: &mut Registry) -> MetaType {
        let fields = &[
            registry
                .field::<Option<String>>("name", i)
                .argument(registry.arg::<Option<bool>>("surname", i)),
            registry.field::<Option<i32>>("iq", i),
            registry.field::<Option<i32>>("numEyes", i),
        ];

        registry
            .build_object_type::<Self>(i, fields)
            .interfaces(&[
                registry.get_type::<Being>(i),
                registry.get_type::<Intelligent>(i),
            ])
            .into_meta()
    }
}

impl GraphQLType for DogOrHuman {
    type Context = ();
    type TypeInfo = ();

    fn name(_: &()) -> Option<&'static str> {
        Some("DogOrHuman")
    }

    fn meta(i: &(), registry: &mut Registry) -> MetaType {
        let types = &[registry.get_type::<Dog>(i), registry.get_type::<Human>(i)];

        registry.build_union_type::<Self>(i, types).into_meta()
    }
}

impl GraphQLType for HumanOrAlien {
    type Context = ();
    type TypeInfo = ();

    fn name(_: &()) -> Option<&'static str> {
        Some("HumanOrAlien")
    }

    fn meta(i: &(), registry: &mut Registry) -> MetaType {
        let types = &[registry.get_type::<Human>(i), registry.get_type::<Alien>(i)];

        registry.build_union_type::<Self>(i, types).into_meta()
    }
}

impl GraphQLType for ComplexInput {
    type Context = ();
    type TypeInfo = ();

    fn name(_: &()) -> Option<&'static str> {
        Some("ComplexInput")
    }

    fn meta(i: &(), registry: &mut Registry) -> MetaType {
        let fields = &[
            registry.arg::<bool>("requiredField", i),
            registry.arg::<Option<i32>>("intField", i),
            registry.arg::<Option<String>>("stringField", i),
            registry.arg::<Option<bool>>("booleanField", i),
            registry.arg::<Option<Vec<Option<String>>>>("stringListField", i),
        ];

        registry
            .build_input_object_type::<Self>(i, fields)
            .into_meta()
    }
}

impl FromInputValue for ComplexInput {
    fn from_input_value(v: &InputValue) -> Option<ComplexInput> {
        let obj = match v.to_object_value() {
            Some(o) => o,
            None => return None,
        };

        Some(ComplexInput {
            required_field: match obj.get("requiredField").and_then(|v| v.convert()) {
                Some(f) => f,
                None => return None,
            },
            int_field: obj.get("intField").and_then(|v| v.convert()),
            string_field: obj.get("stringField").and_then(|v| v.convert()),
            boolean_field: obj.get("booleanField").and_then(|v| v.convert()),
            string_list_field: obj.get("stringListField").and_then(|v| v.convert()),
        })
    }
}

impl GraphQLType for ComplicatedArgs {
    type Context = ();
    type TypeInfo = ();

    fn name(_: &()) -> Option<&'static str> {
        Some("ComplicatedArgs")
    }

    fn meta(i: &(), registry: &mut Registry) -> MetaType {
        let fields = &[
            registry
                .field::<Option<String>>("intArgField", i)
                .argument(registry.arg::<Option<i32>>("intArg", i)),
            registry
                .field::<Option<String>>("nonNullIntArgField", i)
                .argument(registry.arg::<i32>("nonNullIntArg", i)),
            registry
                .field::<Option<String>>("stringArgField", i)
                .argument(registry.arg::<Option<String>>("stringArg", i)),
            registry
                .field::<Option<String>>("booleanArgField", i)
                .argument(registry.arg::<Option<bool>>("booleanArg", i)),
            registry
                .field::<Option<String>>("enumArgField", i)
                .argument(registry.arg::<Option<FurColor>>("enumArg", i)),
            registry
                .field::<Option<String>>("floatArgField", i)
                .argument(registry.arg::<Option<f64>>("floatArg", i)),
            registry
                .field::<Option<String>>("idArgField", i)
                .argument(registry.arg::<Option<ID>>("idArg", i)),
            registry
                .field::<Option<String>>("stringListArgField", i)
                .argument(
                    registry.arg::<Option<Vec<Option<String>>>>("stringListArg", i),
                ),
            registry
                .field::<Option<String>>("complexArgField", i)
                .argument(registry.arg::<Option<ComplexInput>>("complexArg", i)),
            registry
                .field::<Option<String>>("multipleReqs", i)
                .argument(registry.arg::<i32>("req1", i))
                .argument(registry.arg::<i32>("req2", i)),
            registry
                .field::<Option<String>>("multipleOpts", i)
                .argument(registry.arg_with_default("opt1", &0i32, i))
                .argument(registry.arg_with_default("opt2", &0i32, i)),
            registry
                .field::<Option<String>>("multipleOptAndReq", i)
                .argument(registry.arg::<i32>("req1", i))
                .argument(registry.arg::<i32>("req2", i))
                .argument(registry.arg_with_default("opt1", &0i32, i))
                .argument(registry.arg_with_default("opt2", &0i32, i)),
        ];

        registry.build_object_type::<Self>(i, fields).into_meta()
    }
}

impl GraphQLType for QueryRoot {
    type Context = ();
    type TypeInfo = ();

    fn name(_: &()) -> Option<&'static str> {
        Some("QueryRoot")
    }

    fn meta(i: &(), registry: &mut Registry) -> MetaType {
        let fields = &[
            registry
                .field::<Option<Human>>("human", i)
                .argument(registry.arg::<Option<ID>>("id", i)),
            registry.field::<Option<Alien>>("alien", i),
            registry.field::<Option<Dog>>("dog", i),
            registry.field::<Option<Cat>>("cat", i),
            registry.field::<Option<Pet>>("pet", i),
            registry.field::<Option<CatOrDog>>("catOrDog", i),
            registry.field::<Option<DogOrHuman>>("dorOrHuman", i),
            registry.field::<Option<HumanOrAlien>>("humanOrAlien", i),
            registry.field::<Option<ComplicatedArgs>>("complicatedArgs", i),
        ];

        registry.build_object_type::<Self>(i, fields).into_meta()
    }
}

#[macro_export]
macro_rules! validate {
    ($r:expr, $q:expr, $factory:expr) => {
        {
            let mut root = $crate::schema::model::RootNode::new($r, $crate::types::scalars::EmptyMutation::<()>::new());

            {
                let schema = ::std::sync::Arc::get_mut(&mut root.schema).expect("Schema not uniquely pointed to");

                schema.add_directive($crate::schema::model::DirectiveType::new(
                    "onQuery",
                    &[$crate::schema::model::DirectiveLocation::Query],
                    &[],
                ));
                schema.add_directive($crate::schema::model::DirectiveType::new(
                    "onMutation",
                    &[$crate::schema::model::DirectiveLocation::Mutation],
                    &[],
                ));
                schema.add_directive($crate::schema::model::DirectiveType::new(
                    "onField",
                    &[$crate::schema::model::DirectiveLocation::Field],
                    &[],
                ));
                schema.add_directive($crate::schema::model::DirectiveType::new(
                    "onFragmentDefinition",
                    &[$crate::schema::model::DirectiveLocation::FragmentDefinition],
                    &[],
                ));
                schema.add_directive($crate::schema::model::DirectiveType::new(
                    "onFragmentSpread",
                    &[$crate::schema::model::DirectiveLocation::FragmentSpread],
                    &[],
                ));
                schema.add_directive($crate::schema::model::DirectiveType::new(
                    "onInlineFragment",
                    &[$crate::schema::model::DirectiveLocation::InlineFragment],
                    &[],
                ));
            }

            let q = $q;
            let source = $crate::shared_str::SharedStr::clone_from_str(q);
            let doc = $crate::parser::parse_document_source(source).expect(&format!("Parse error on input {:#?}", q));
            let mut ctx = $crate::validation::ValidatorContext::new(&root.schema, &doc);

            {
                let mut mv = $crate::validation::MultiVisitor::with($crate::validation::MultiVisitorNil, $factory());
                $crate::validation::visit(&mut mv, &mut ctx, &doc);
            }

            ctx.into_errors()
        }
    }
}

#[macro_export]
macro_rules! expect_passes_rule {
    ($factory:expr, $q:expr $(,)*) => {
        expect_passes_rule!($crate::validation::test_harness::QueryRoot, $factory, $q);
    };
    ($r:expr, $factory:expr, $q:expr $(,)*) => {
        {
            let errs = validate!($r, $q, $factory);

            if !errs.is_empty() {
                $crate::validation::test_harness::print_errors(&errs);
                panic!("Expected rule to pass, but errors found");
            }
        }
    }
}

#[macro_export]
macro_rules! expect_fails_rule {
    ($factory:expr, $q:expr, $expected_errors:expr $(,)*) => {
        expect_fails_rule!($crate::validation::test_harness::QueryRoot, $factory, $q, $expected_errors);
    };
    ($r:expr, $factory:expr, $q:expr, $expected_errors:expr $(,)*) => {
        let errs = validate!($r, $q, $factory);
        let expected_errors = $expected_errors;

        if errs.is_empty() {
            panic!("Expected rule to fail, but no errors were found");
        } else if errs != expected_errors {
            println!("==> Expected errors:");
            $crate::validation::test_harness::print_errors(expected_errors);

            println!("\n==> Actual errors:");
            $crate::validation::test_harness::print_errors(&errs);

            panic!("Unexpected set of errors found");
        }
    }
}

pub fn print_errors(errs: &[RuleError]) {
    for err in errs {
        for p in err.locations() {
            print!("[{:>3},{:>3},{:>3}]  ", p.index(), p.line(), p.column());
        }
        println!("{}", err.message());
    }
}
