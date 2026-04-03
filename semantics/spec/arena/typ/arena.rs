use crate::{spec::arena::typ::PrimitiveTy, text::StringId};

/// Data of option type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct OptionData<Id> {
    /// Id of the option's inner type
    pub inner_id: Id,
}

/// Data of array type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ArrayData<Id> {
    /// Id of the array's inner type
    pub inner_id: Id,
}

/// Data of enum type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct EnumData {
    /// Type of the enum members
    pub typ: EnumTy,

    // Range of enum members
    first: u32,
    end: u32,
}

/// Data for member of the enum
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct EnumMemberData {
    /// Value of the member
    pub value: ValueId,

    /// Documentation for the member
    pub docs: Option<StringId>,
}

/// Data for response type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ResponseData {
    body: InlineTyId,
    headers: Option<SimpleRecordReferenceId>,

    // TODO: Mime type
    content_type: Option<()>,
}

/// Range of the fields in the record
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct RecordRange {
    first: u32,
    end: u32,
}

/// Range of the fields in the union
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct UnionRange {
    first: u32,
    end: u32,
}

// Data of a inline type in arena
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum InlineTypeData<Id> {
    Primitive(PrimitiveTy),
    Reference(StringId), // TODO: Reference data instead of stringid
    Option(OptionData<Id>),
    Array(ArrayData<Id>),
}

/// Data of a type in arena.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TypeData {
    Inline(InlineTypeData),
    Enum(EnumData),
    Record(RecordRange),
    Union(UnionRange),
}

/// Data of a root type in arena.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum RootTypeData {
    Type(TypeData),
    Response(ResponseData),
}

/// Range of the fields in a simple record
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct SimpleRecordRange(RecordRange);
