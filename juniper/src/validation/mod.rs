//! Query validation related methods and data structures

#[cfg(test)]
#[macro_use]
mod test_harness;

mod visitor;
mod traits;
mod context;
mod multi_visitor;
mod rules;
mod input_value;

pub use self::traits::Visitor;
pub use self::visitor::visit;
pub use self::context::{RuleError, ValidatorContext};
pub use self::rules::visit_all_rules;
pub use self::multi_visitor::{MultiVisitor, MultiVisitorNil};
pub use self::input_value::validate_input_values;
