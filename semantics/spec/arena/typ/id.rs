/// Id of an inline type stored in the arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct InlineTypeId(pub(super) u32);

/// Id of a type stored in the arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct TypeId(pub(super) u32);

/// Id of a response type stored in the [`TypeArena`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ResponseTypeId(pub(super) u32);

/// Id of any type including response.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct RootTypeId(pub(super) u32);

impl From<TypeId> for RootTypeId {
    fn from(value: TypeId) -> Self {
        Self(value.0)
    }
}

impl From<ResponseTypeId> for RootTypeId {
    fn from(value: ResponseTypeId) -> Self {
        Self(value.0)
    }
}

/// Id of a field in a record or union.
///
/// Used for getting a specific field through the context.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct TypeFieldId {
    pub(super) container_id: TypeId,

    /// Index of the slot in the arena
    pub(super) slot_idx: u32,
}

/// Id of a simple type stored in the arena
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct SimpleTypeId(u32);

impl From<SimpleTypeId> for TypeId {
    fn from(value: SimpleTypeId) -> Self {
        Self(value.0)
    }
}

/// Id of a named reference to stored in the [`TypeArena`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct SimpleRecordReferenceId(u32);

impl From<SimpleRecordReferenceId> for TypeId {
    fn from(value: SimpleRecordReferenceId) -> Self {
        Self(value.0)
    }
}

/// Id of a named reference to stored in the [`TypeArena`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ResponseReferenceId(u32);

impl From<ResponseReferenceId> for RootTypeId {
    fn from(value: ResponseReferenceId) -> Self {
        Self(value.0)
    }
}
