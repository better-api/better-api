mod source_map;
mod string;
mod text;

pub mod oracle;
pub mod typ;
pub mod value;

/// Re-export for simpler use case.
pub use oracle::Oracle;

/// Semantic element.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Element {
    Value(value::ValueId),
    Type(typ::TypeId),
    TypeDefinition(string::StringId),
}
