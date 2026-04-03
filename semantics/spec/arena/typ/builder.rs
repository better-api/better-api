/// Reference to a named [`Type`] definition.
///
/// Validated by the analyzer before insertion into the arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub(crate) struct TypeRef(pub StringId);

/// Reference to a named [`RootType`] definition.
///
/// Validated by the analyzer before insertion into the arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub(crate) struct RootRef(pub StringId);

impl From<TypeRef> for RootRef {
    fn from(value: TypeRef) -> Self {
        Self(value.0)
    }
}
