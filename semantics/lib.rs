mod source_map;
mod text;

pub mod oracle;
pub mod typ;
pub mod value;

/// Re-export for simpler use case.
pub use oracle::Oracle;

/// Id of an interned string.
pub type StringId = string_interner::DefaultSymbol;

/// Semantic element.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Element {
    Value(value::ValueId),
    Type(typ::TypeId),
    TypeDefinition(StringId),
}
