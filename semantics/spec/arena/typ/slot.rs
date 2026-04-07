use crate::spec::arena::typ::id::{InlineTypeId, SimpleRecordReferenceId};
use crate::spec::arena::typ::{EnumTy, PrimitiveTy};
use crate::spec::arena::value::ValueId;
use crate::text::{NameId, StringId};

/// Slot in the type arena.
///
/// Array and Option can be nested to form complex inline types
/// (for example `[[[i32?]]?]?`). They can also appear as record/union
/// field types, so we need an efficient way to skip the whole nesting
/// when iterating fields.
///
/// Array and Option work by storing the wrapped type in the next slot.
/// For example, `[i32]` is `[Slot::Array, Slot::Primitive(_)]`.
/// The `end` field marks the slot after the whole nesting so field
/// iteration can skip the full inline tree.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) enum Slot<Ref> {
    Primitive(PrimitiveTy),
    Reference(Ref),
    Option {
        // Index after the last type in the nested Option<...> type.
        // Used for skipping the whole Option during iteration.
        end: u32,
    },
    Array {
        // Index after the last type in the nested Array<...> type.
        // Used for skipping the whole Array during iteration.
        end: u32,
    },
    Enum {
        /// Type of the enum (string, i32, ...)
        typ: EnumTy,

        // Index after the last enum member.
        // Used for skipping the whole Enum during iteration.
        end: u32,
    },
    EnumMember {
        value: ValueId,
        docs: Option<StringId>,
    },
    Response {
        body: InlineTypeId,
        headers: Option<SimpleRecordReferenceId>,
        content_type: Option<()>, // TODO: Mime type
    },
    Record {
        // Index after the last field in the record.
        // Used for skipping the whole record during iteration.
        end: u32,
    },
    Union {
        // Index after the last field in the union.
        // Used for skipping the whole record during iteration.
        end: u32,
    },
    // Name of the field in record or union.
    // In TrackedSlot syntax pointer points to the whole field
    // and not just the name.
    TypeField {
        name: NameId,
        default: Option<ValueId>,
        docs: Option<StringId>,
    },
}

impl<Ref> From<PrimitiveTy> for Slot<Ref> {
    fn from(value: PrimitiveTy) -> Self {
        Self::Primitive(value)
    }
}

impl<Ref> Slot<Ref> {
    /// Maps the reference payload while preserving the slot shape.
    ///
    /// Applies `map` only to [`Slot::Reference`]. All other variants are
    /// returned unchanged.
    ///
    /// Returns the error from `map` if reference mapping fails.
    pub(super) fn map_ref<MappedRef, E>(
        self,
        map: impl FnOnce(Ref) -> Result<MappedRef, E>,
    ) -> Result<Slot<MappedRef>, E> {
        let slot = match self {
            Slot::Primitive(primitive) => Slot::Primitive(primitive),
            Slot::Reference(reference) => Slot::Reference(map(reference)?),
            Slot::Option { end } => Slot::Option { end },
            Slot::Array { end } => Slot::Array { end },
            Slot::Enum { typ, end } => Slot::Enum { typ, end },
            Slot::EnumMember { value, docs } => Slot::EnumMember { value, docs },
            Slot::Response {
                body,
                headers,
                content_type,
            } => Slot::Response {
                body,
                headers,
                content_type,
            },
            Slot::Record { end } => Slot::Record { end },
            Slot::Union { end } => Slot::Union { end },
            Slot::TypeField {
                name,
                default,
                docs,
            } => Slot::TypeField {
                name,
                default,
                docs,
            },
        };

        Ok(slot)
    }
}
