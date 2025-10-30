mod text;

pub mod typ;
pub mod value;

/// Id for strings
type StringId = string_interner::DefaultSymbol;

/// Semantic element.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Element {
    String(StringId),
    Value(value::ValueId),
    ObjectField(value::ObjectFieldId),
    Type(typ::TypeId),
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

impl From<value::ObjectFieldId> for Element {
    fn from(value: value::ObjectFieldId) -> Self {
        Self::ObjectField(value)
    }
}

impl From<typ::TypeId> for Element {
    fn from(value: typ::TypeId) -> Self {
        Self::Type(value)
    }
}
