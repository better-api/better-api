use crate::spec::arena::typ::id::{
    InlineTypeId, ResponseTypeId, RootTypeId, SimpleRecordReferenceId, TypeFieldId, TypeId,
};
use crate::spec::arena::typ::slot::Slot;
use crate::spec::arena::typ::{EnumTy, PrimitiveTy};
use crate::spec::arena::value::ValueId;
use crate::spec::SymbolTable;
use crate::text::{NameId, StringId};

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
    pub body: InlineTypeId,
    pub headers: Option<SimpleRecordReferenceId>,

    // TODO: Mime type
    pub content_type: Option<()>,
}

impl ResponseData {
    fn from_slot(slot: Slot<ReferenceSlot>) -> Option<Self> {
        let Slot::Response {
            body,
            headers,
            content_type,
        } = slot
        else {
            return None;
        };

        Some(ResponseData {
            body,
            headers,
            content_type,
        })
    }
}

/// Range of the fields in the record
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct RecordRange {
    id: TypeId,
    first: u32,
    end: u32,
}

/// Range of the fields in the union
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct UnionRange {
    id: TypeId,
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

impl InlineTypeData {
    fn from_slot(slot: Slot<ReferenceSlot>, idx: u32) -> Option<Self> {
        let res = match slot {
            Slot::Primitive(prim) => InlineTypeData::Primitive(prim),
            Slot::Reference(r) => InlineTypeData::Reference(ReferenceData {
                name: r.name,
                target: TypeId(r.target),
            }),
            Slot::Option { .. } => InlineTypeData::Option(OptionData {
                inner_id: InlineTypeId(idx + 1),
            }),
            Slot::Array { end } => InlineTypeData::Array(ArrayData {
                inner_id: InlineTypeId(idx + 1),
            }),
            _ => return None,
        };

        Some(res)
    }
}

/// Data of a type in arena.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TypeData {
    Inline(InlineTypeData),
    Enum(EnumData),
    Record(RecordRange),
    Union(UnionRange),
}

impl TypeData {
    fn from_slot(slot: Slot<ReferenceSlot>, idx: u32) -> Option<Self> {
        let res = match slot {
            Slot::Primitive(_) | Slot::Reference(_) | Slot::Option { .. } | Slot::Array { .. } => {
                TypeData::Inline(InlineTypeData::from_slot(slot, idx)?)
            }

            Slot::Enum { typ, end } => TypeData::Enum(EnumData {
                typ,
                first: idx + 1,
                end,
            }),
            Slot::Record { end } => TypeData::Record(RecordRange {
                id: TypeId(idx),
                first: idx + 1,
                end,
            }),
            Slot::Union { end } => TypeData::Union(UnionRange {
                id: TypeId(idx),
                first: idx + 1,
                end,
            }),

            _ => return None,
        };

        Some(res)
    }
}

/// Data of a root type in arena.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum RootTypeData {
    Type(TypeData),
    Response(ResponseData),
    Reference(ReferenceData<RootTypeId>),
}

impl RootTypeData {
    fn from_slot(slot: Slot<ReferenceSlot>, idx: u32) -> Option<RootTypeData> {
        let res = match slot {
            Slot::Reference(r) => RootTypeData::Reference(ReferenceData {
                name: r.name,
                target: RootTypeId(r.target),
            }),

            Slot::Primitive(_)
            | Slot::Option { .. }
            | Slot::Array { .. }
            | Slot::Enum { .. }
            | Slot::Record { .. }
            | Slot::Union { .. } => RootTypeData::Type(TypeData::from_slot(slot, idx)?),

            Slot::Response {
                body,
                headers,
                content_type,
            } => RootTypeData::Response(ResponseData::from_slot(slot)?),

            _ => return None,
        };

        Some(res)
    }
}

/// Helper data structure for type field data.
#[derive(Debug, Clone, PartialEq)]
struct TypeFieldData {
    id: TypeFieldId,
    name: NameId,
    default: Option<ValueId>,
    docs: Option<StringId>,
    typ: InlineTypeData,
}

/// Data of a field in a record
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct RecordFieldData {
    pub id: TypeFieldId,
    pub name: NameId,
    pub default: Option<ValueId>,
    pub docs: Option<StringId>,
    pub typ: InlineTypeData,
}

impl From<TypeFieldData> for RecordFieldData {
    fn from(value: TypeFieldData) -> Self {
        Self {
            id: value.id,
            name: value.name,
            default: value.default,
            docs: value.docs,
            typ: value.typ,
        }
    }
}

/// Data of a field in a union
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct UnionFieldData {
    pub id: TypeFieldId,
    pub name: NameId,
    pub docs: Option<StringId>,
    pub typ: InlineTypeData,
}

impl From<TypeFieldData> for UnionFieldData {
    fn from(value: TypeFieldData) -> Self {
        Self {
            id: value.id,
            name: value.name,
            docs: value.docs,
            typ: value.typ,
        }
    }
}

/// Cursor used for iterating through enum members in the arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct EnumCursor {
    next: u32,
    end: u32,
}

/// Internal cursor used for iterating through an type with fields in the arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct TypeFieldCursor {
    container_id: TypeId,
    next: u32,
    end: u32,
}

/// Cursor used for iterating over fields in a record
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct RecordCursor(TypeFieldCursor);

/// Cursor used for iterating over fields in a union
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct UnionCursor(TypeFieldCursor);

#[derive(Debug, Clone, Copy, PartialEq)]
struct ReferenceSlot {
    name: StringId,
    target: u32,
}

/// Type arena that holds semantic types
#[derive(Debug, Clone)]
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

    pub(crate) fn get_inline_type(&self, id: InlineTypeId) -> InlineTypeData {
        let slot = self.data[id.0 as usize];
        InlineTypeData::from_slot(slot, id.0).expect("invalid slot for InlineTypeId")
    }

    pub(crate) fn get_type(&self, id: TypeId) -> TypeData {
        let slot = self.data[id.0 as usize];
        TypeData::from_slot(slot, id.0).expect("invalid slot for TypeId")
    }

    pub(crate) fn get_response(&self, id: ResponseTypeId) -> ResponseData {
        let slot = self.data[id.0 as usize];
        ResponseData::from_slot(slot).expect("invalid slot for ResponseTypeId")
    }

    pub(crate) fn get_root_type(&self, id: RootTypeId) -> RootTypeData {
        let slot = self.data[id.0 as usize];
        RootTypeData::from_slot(slot, id.0).expect("invalid slot for RootTypeId")
    }

    /// Get cursor pointing to the first member of the enum
    pub(crate) fn enum_cursor(&self, enm: EnumData) -> EnumCursor {
        EnumCursor {
            next: enm.first,
            end: enm.end,
        }
    }

    /// Get next member in the enum.
    ///
    /// If cursor is pointing to the end of the enum, None is returned.
    pub(crate) fn next_enum_member(&self, c: EnumCursor) -> Option<(EnumMemberData, EnumCursor)> {
        if c.next >= c.end {
            return None;
        }

        let Slot::EnumMember { value, docs } = self.data[c.next as usize] else {
            unreachable!("invalid enum member slot at {:?}", c.next)
        };

        let next = EnumCursor {
            next: c.next + 1,
            end: c.end,
        };
        let member = EnumMemberData { value, docs };
        Some((member, next))
    }

    /// Get cursor pointing to the first field in the record
    pub(crate) fn record_cursor(&self, rec: RecordRange) -> RecordCursor {
        RecordCursor(TypeFieldCursor {
            container_id: rec.id,
            next: rec.first,
            end: rec.end,
        })
    }

    /// Get cursor pointing to the first field in the union
    pub(crate) fn union_cursor(&self, union: UnionRange) -> UnionCursor {
        UnionCursor(TypeFieldCursor {
            container_id: union.id,
            next: union.first,
            end: union.end,
        })
    }

    /// Helper function for getting next field in record and union.
    fn next_type_field(&self, c: TypeFieldCursor) -> Option<(TypeFieldData, TypeFieldCursor)> {
        if c.next >= c.end {
            return None;
        }

        // Get field id
        let field_id = TypeFieldId {
            container_id: c.container_id,
            slot_idx: c.next,
        };

        // Get field info
        let Slot::TypeField {
            name,
            default,
            docs,
        } = self.data[c.next as usize]
        else {
            unreachable!("invalid type field slot at {:?}", c.next)
        };

        // Get type of the field.
        let typ_slot = self.data[c.next as usize + 1];
        let typ = InlineTypeData::from_slot(typ_slot, c.next + 1)
            .expect("invalid type field's type slot");

        // Get next cursor position
        let next_idx = match typ_slot {
            Slot::Primitive(_) | Slot::Reference(_) => c.next + 2,
            Slot::Option { end } | Slot::Array { end } => end,
            _ => unreachable!("invalid type field's type slot at {:?}", c.next + 1),
        };

        // Return the field
        let field_data = TypeFieldData {
            id: field_id,
            name,
            default,
            docs,
            typ,
        };
        let next = TypeFieldCursor {
            container_id: c.container_id,
            next: next_idx,
            end: c.end,
        };
        Some((field_data, next))
    }

    /// Get next field in the record
    ///
    /// If cursor is pointing to the end of the record, None is returned.
    pub(crate) fn next_record_field(
        &self,
        c: RecordCursor,
    ) -> Option<(RecordFieldData, RecordCursor)> {
        let (data, next) = self.next_type_field(c.0)?;
        Some((data.into(), RecordCursor(next)))
    }

    /// Get next field in the union.
    ///
    /// If cursor is pointing of the end of the record, None is returned
    pub(crate) fn next_union_field(&self, c: UnionCursor) -> Option<(UnionFieldData, UnionCursor)> {
        let (data, next) = self.next_type_field(c.0)?;
        Some((data.into(), UnionCursor(next)))
    }
}
