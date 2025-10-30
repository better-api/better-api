mod text;

// pub mod typ;
pub mod value;

/// Id for strings
type StringId = string_interner::DefaultSymbol;

/// Semantic element.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Element {
    String(StringId),
    Value(value::ValueId),
}

impl From<StringId> for Element {
    fn from(value: StringId) -> Self {
        Self::String(value)
    }
}

impl From<value::ValueId> for Element {
    fn from(value: value::ValueId) -> Self {
        Self::Value(value)
    }
}
