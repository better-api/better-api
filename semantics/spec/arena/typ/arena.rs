use std::any::TypeId;

use crate::spec::SymbolTable;
use crate::spec::arena::typ::id::{InlineTypeId, RootTypeId, SimpleRecordReferenceId};
use crate::spec::arena::typ::slot::Slot;
use crate::spec::arena::typ::{EnumTy, PrimitiveTy};
use crate::spec::arena::value::ValueId;
use crate::text::StringId;

/// Data of option type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct OptionData {
    /// Id of the option's inner type
    pub inner_id: InlineTypeId,
}

/// Data of array type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ArrayData {
    /// Id of the array's inner type
    pub inner_id: InlineTypeId,
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
    body: InlineTypeId,
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

/// Data for a reference to target.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ReferenceData<Id> {
    pub name: StringId,
    pub target: Id,
}

// Data of a inline type in arena
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum InlineTypeData {
    Primitive(PrimitiveTy),
    Reference(ReferenceData<TypeId>),
    Option(OptionData),
    Array(ArrayData),
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
    Reference(ReferenceData<RootTypeId>),
}

#[derive(Debug, Clone, PartialEq)]
struct ReferenceSlot {
    name: StringId,
    target: u32,
}

/// Type arena that holds semantic types
pub(crate) struct TypeArena {
    data: Vec<Slot<ReferenceSlot>>,
}

impl TypeArena {
    /// Constructs arena from data in [`TypeArenaBuilder`](super::builder::TypeArenaBuilder).
    ///
    /// **Note:** This function shouldn't be called directly. Instead
    /// [`TypeArenaBuilder::finish`](super::builder::TypeArenaBuilder::finish) should be called.
    ///
    /// If any of the references in the builder are not valid, an error is returned. The error
    /// is the name of the reference that couldn't be resolved.
    pub(super) fn from_builder_data(
        data: Vec<Slot<StringId>>,
        table: &SymbolTable,
    ) -> Result<Self, StringId> {
        let final_data: Result<Vec<Slot<ReferenceSlot>>, StringId> = data
            .into_iter()
            .map(|slot| {
                slot.map_ref(|name| {
                    if let Some(def) = table.get(&name) {
                        Ok(ReferenceSlot {
                            name,
                            target: def.typ.0,
                        })
                    } else {
                        Err(name)
                    }
                })
            })
            .collect();

        Ok(Self { data: final_data? })
    }
}
