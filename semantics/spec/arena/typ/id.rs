/// Id of an inline type stored in the arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct InlineTypeId(pub(super) u32);

#[cfg(test)]
impl InlineTypeId {
    pub(crate) fn from_root_type_id(id: RootTypeId) -> Self {
        Self(id.0)
    }
}

impl From<InlineTypeId> for TypeId {
    fn from(value: InlineTypeId) -> Self {
        Self(value.0)
    }
}

/// Id of a type stored in the arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct TypeId(pub(super) u32);

/// Id of a response type stored in the [`TypeArena`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ResponseTypeId(pub(super) u32);

/// Id of any type including response.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct RootTypeId(pub(super) u32);

impl From<InlineTypeId> for RootTypeId {
    fn from(value: InlineTypeId) -> Self {
        Self(value.0)
    }
}

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

/// Proof that the type at this arena position was validated
/// as a simple record behind a reference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct SimpleRecordReferenceProof(TypeId);

impl SimpleRecordReferenceProof {
    /// Creates new proof that the type is actually a reference to
    /// simple record type.
    ///
    /// It's the caller's responsibility to check that this is actually true.
    /// Constructing a proof with wrong type, and using it, will lead to panics!
    pub(crate) fn new(id: TypeId) -> Self {
        Self(id)
    }

    /// Returns id of the type behind the proof.
    pub(crate) fn id(&self) -> TypeId {
        self.0
    }
}

/// Proof that the type at this arena position was validated
/// as a response behind a reference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ResponseReferenceProof(RootTypeId);

impl ResponseReferenceProof {
    /// Creates new proof that the type is actually a reference to
    /// a response.
    ///
    /// It's the caller's responsibility to check that this is actually true.
    /// Constructing a proof with wrong type, and using it, will lead to panics!
    pub(crate) fn new(id: RootTypeId) -> Self {
        Self(id)
    }

    /// Returns id of the type behind the proof.
    pub(crate) fn id(&self) -> RootTypeId {
        self.0
    }
}
