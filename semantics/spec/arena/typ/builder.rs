use crate::mime::MimeRangeId;
use crate::spec::SymbolTable;
use crate::spec::arena::typ::arena::TypeArena;
use crate::spec::arena::typ::id::{
    InlineTypeId, ResponseTypeId, RootTypeId, SimpleRecordReferenceProof, TypeFieldId, TypeId,
};
use crate::spec::arena::typ::slot::Slot;
use crate::spec::arena::typ::{EnumTy, PrimitiveTy};
use crate::spec::arena::value::ValueId;
use crate::text::{NameId, StringId};

/// Reference to a named non-response type definition.
///
/// Validated by the analyzer before insertion into the arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub(crate) struct TypeRef(pub(crate) StringId);

/// Reference to a named root type definition.
///
/// Validated by the analyzer before insertion into the arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub(crate) struct RootRef(pub(crate) StringId);

impl From<TypeRef> for RootRef {
    fn from(value: TypeRef) -> Self {
        Self(value.0)
    }
}

/// Helper type for adding records and unions to arena.
///
/// Constructed via [`TypeArenaBuilder::start_record`] or
/// [`TypeArenaBuilder::start_union`].
/// Call [`finish`](FieldBuilder::finish) once all fields are added.
///
/// Dropping the builder without finishing rolls back any changes.
pub(crate) struct FieldBuilder<'p> {
    data: &'p mut Vec<Slot<StringId>>,

    /// Index in the arena that contains Slot::Record or Slot::Union.
    start: TypeId,

    /// Was finished called, used by drop implementation.
    finished: bool,
}

impl<'p> FieldBuilder<'p> {
    fn new_record(arena: &'p mut TypeArenaBuilder) -> Self {
        let idx = arena.data.len();
        arena.data.push(Slot::Record { end: 0 });

        Self {
            data: &mut arena.data,
            start: TypeId(idx as u32),
            finished: false,
        }
    }

    fn new_union(arena: &'p mut TypeArenaBuilder) -> Self {
        let idx = arena.data.len();
        arena.data.push(Slot::Union { end: 0 });

        Self {
            data: &mut arena.data,
            start: TypeId(idx as u32),
            finished: false,
        }
    }

    /// Add a new field with a primitive type.
    ///
    /// `default` is a default value stored in the [`value::ValueArena`].
    pub(crate) fn add_primitive(
        &mut self,
        name: NameId,
        typ: PrimitiveTy,
        default: Option<ValueId>,
        docs: Option<StringId>,
    ) -> TypeFieldId {
        let slot_idx = self.data.len() as u32;

        self.data.push(Slot::TypeField {
            name,
            default,
            docs,
        });
        self.data.push(typ.into());

        TypeFieldId {
            container_id: self.start,
            slot_idx,
        }
    }

    /// Add a new field with a reference type.
    ///
    /// Reference type must point to a "normal" type and not a response.
    /// `default` is a default value stored in the [`value::ValueArena`].
    pub(crate) fn add_reference(
        &mut self,
        name: NameId,
        reference: TypeRef,
        default: Option<ValueId>,
        docs: Option<StringId>,
    ) -> TypeFieldId {
        let slot_idx = self.data.len() as u32;

        self.data.push(Slot::TypeField {
            name,
            default,
            docs,
        });
        self.data.push(Slot::Reference(reference.0));

        TypeFieldId {
            container_id: self.start,
            slot_idx,
        }
    }

    /// Add a new field of array type.
    ///
    /// Returns array builder and [`TypeFieldId`] of the created field.
    /// If returned builder, this builder or any parent builder is dropped before
    /// calling `.finish()` on it, the returned [`TypeFieldId`] will be invalid!
    pub(crate) fn start_array<'a>(
        &'a mut self,
        name: NameId,
        default: Option<ValueId>,
        docs: Option<StringId>,
    ) -> (OptionArrayBuilder<'a>, TypeFieldId) {
        let slot_idx = self.data.len() as u32;

        self.data.push(Slot::TypeField {
            name,
            default,
            docs,
        });

        let field_id = TypeFieldId {
            container_id: self.start,
            slot_idx,
        };
        let builder = OptionArrayBuilder::new_array(self.data, Some(slot_idx));

        (builder, field_id)
    }

    /// Add a new field of option type.
    ///
    /// Returns option builder and [`TypeFieldId`] of the created field.
    /// If returned builder, this builder or any parent builder is dropped before
    /// calling `.finish()` on it, the returned [`TypeFieldId`] will be invalid!
    pub(crate) fn start_option<'a>(
        &'a mut self,
        name: NameId,
        default: Option<ValueId>,
        docs: Option<StringId>,
    ) -> (OptionArrayBuilder<'a>, TypeFieldId) {
        let slot_idx = self.data.len() as u32;

        self.data.push(Slot::TypeField {
            name,
            default,
            docs,
        });

        let field_id = TypeFieldId {
            container_id: self.start,
            slot_idx,
        };
        let builder = OptionArrayBuilder::new_option(self.data, Some(slot_idx));

        (builder, field_id)
    }

    /// Finalize the record or union currently being built.
    ///
    /// Returns the id of the composite type inside the arena.
    pub(crate) fn finish(mut self) -> TypeId {
        self.finished = true;

        let idx = self.data.len() as u32;

        let start = self.start.0 as usize;
        let head = &mut self.data[start];
        match head {
            Slot::Record { end } => *end = idx,
            Slot::Union { end, .. } => *end = idx,
            _ => unreachable!("FieldBuilder::start must point to Slot::Record or Slot::Union"),
        }

        self.start
    }
}

impl<'a> Drop for FieldBuilder<'a> {
    fn drop(&mut self) {
        if self.finished {
            return;
        }

        let len = self.start.0 as usize;
        self.data.truncate(len);
    }
}

/// Helper type for adding Array and Option type to arena.
///
/// This type is constructed with [`TypeArenaBuilder::start_array`],
/// [`TypeArenaBuilder::start_option`], [`FieldBuilder::start_array`], or
/// [`FieldBuilder::start_option`].
/// After you are done, call [`OptionArrayBuilder::finish_primitive`] or
/// [`OptionArrayBuilder::finish_reference`], which returns the id of the
/// final array or option in the arena.
///
/// If builder is dropped before calling finish, added types are removed from the arena.
pub(crate) struct OptionArrayBuilder<'p> {
    data: &'p mut Vec<Slot<StringId>>,

    /// Optionally clean up the data if the type is not finished successfully.
    truncate: Option<u32>,

    /// Index in the arena that contains the first Slot::Array or Slot::Option.
    start: InlineTypeId,

    /// Was finished called, used by drop implementation.
    finished: bool,
}

impl<'p> OptionArrayBuilder<'p> {
    fn new_array(data: &'p mut Vec<Slot<StringId>>, truncate: Option<u32>) -> Self {
        let idx = data.len();
        data.push(Slot::Array { end: 0 });

        Self {
            data,
            truncate,
            start: InlineTypeId(idx as u32),
            finished: false,
        }
    }

    fn new_option(data: &'p mut Vec<Slot<StringId>>, truncate: Option<u32>) -> Self {
        let idx = data.len();
        data.push(Slot::Option { end: 0 });

        Self {
            data,
            truncate,
            start: InlineTypeId(idx as u32),
            finished: false,
        }
    }

    /// Start a new array inside the current array or option.
    ///
    /// Returns id of the array type that was started. If the builder
    /// is not finished, the returned id is invalid.
    pub(crate) fn start_array(&mut self) -> InlineTypeId {
        let idx = self.data.len();
        self.data.push(Slot::Array { end: 0 });
        InlineTypeId(idx as u32)
    }

    /// Start a new option inside the current array or option.
    ///
    /// Returns id of the option type that was started. If the builder
    /// is not finished, the returned id is invalid.
    pub(crate) fn start_option(&mut self) -> InlineTypeId {
        let idx = self.data.len();
        self.data.push(Slot::Option { end: 0 });
        InlineTypeId(idx as u32)
    }

    /// Helper function for finishing
    fn finish(mut self, slot: Slot<StringId>) -> InlineTypeId {
        self.finished = true;

        let start = self.start.0 as usize;
        let len = self.data.len();
        let end_id = len as u32 + 1;

        // Update the in between types
        for slot in &mut self.data[start..len] {
            match slot {
                Slot::Option { end } => *end = end_id,
                Slot::Array { end } => *end = end_id,
                _ => unreachable!(
                    "OptionArrayBuilder must contain only Slot::Option and Slot::Array before the final slot, got {slot:?}"
                ),
            }
        }

        self.data.push(slot);

        self.start
    }

    /// Finish building this type with a primitive type.
    pub(crate) fn finish_primitive(self, typ: PrimitiveTy) -> InlineTypeId {
        self.finish(typ.into())
    }

    /// Finish building this with a reference.
    ///
    /// Reference must point to a "normal" type and not a response.
    pub(crate) fn finish_reference(self, reference: TypeRef) -> InlineTypeId {
        self.finish(Slot::Reference(reference.0))
    }
}

impl<'p> Drop for OptionArrayBuilder<'p> {
    fn drop(&mut self) {
        if self.finished {
            return;
        }

        // Truncate the builder to `truncate` parameter, or self start.
        let len = self.truncate.unwrap_or(self.start.0);
        self.data.truncate(len as usize);
    }
}

/// Helper type for adding enums to arena.
///
/// Constructed via [`TypeArenaBuilder::start_enum`].
/// Call [`finish`](EnumBuilder::finish) once all members are added.
///
/// Dropping the builder without finishing rolls back any changes.
pub(crate) struct EnumBuilder<'p> {
    data: &'p mut Vec<Slot<StringId>>,

    /// Index in the arena that contains Slot::Enum
    start: TypeId,

    /// Was finished called, used by drop implementation.
    finished: bool,
}

impl<'p> EnumBuilder<'p> {
    fn new(data: &'p mut Vec<Slot<StringId>>, typ: EnumTy) -> Self {
        let idx = data.len();
        data.push(Slot::Enum { typ, end: 0 });

        Self {
            data,
            start: TypeId(idx as u32),
            finished: false,
        }
    }

    /// Add new member to the enum.
    pub(crate) fn add_member(&mut self, value: ValueId, docs: Option<StringId>) {
        self.data.push(Slot::EnumMember { value, docs });
    }

    /// Finish building the enum
    pub(crate) fn finish(mut self) -> TypeId {
        self.finished = true;

        let idx = self.data.len() as u32;

        let start = self.start.0 as usize;
        let head = &mut self.data[start];
        match head {
            Slot::Enum { end, .. } => *end = idx,
            _ => unreachable!("EnumBuilder::start must point to Slot::Enum"),
        }

        self.start
    }
}

impl<'p> Drop for EnumBuilder<'p> {
    fn drop(&mut self) {
        if self.finished {
            return;
        }

        let len = self.start.0 as usize;
        self.data.truncate(len);
    }
}

/// Builder for the arena that holds semantic types.
#[derive(Debug, Clone, Default, PartialEq)]
pub(crate) struct TypeArenaBuilder {
    data: Vec<Slot<StringId>>,
}

impl TypeArenaBuilder {
    /// Add a primitive type to the arena.
    ///
    /// Returns the [`InlineTypeId`] assigned to the new type.
    pub(crate) fn add_primitive(&mut self, typ: PrimitiveTy) -> InlineTypeId {
        let idx = self.data.len();
        self.data.push(typ.into());
        InlineTypeId(idx as u32)
    }

    /// Add a reference to the arena.
    ///
    /// Returns the [`RootTypeId`] assigned to the new type.
    pub(crate) fn add_root_reference(&mut self, reference: RootRef) -> RootTypeId {
        let idx = self.data.len();
        self.data.push(Slot::Reference(reference.0));
        RootTypeId(idx as u32)
    }

    /// Add a reference to the arena.
    ///
    /// Returns the [`InlineTypeId`] assigned to the new type.
    pub(crate) fn add_reference(&mut self, reference: TypeRef) -> InlineTypeId {
        let idx = self.data.len();
        self.data.push(Slot::Reference(reference.0));
        InlineTypeId(idx as u32)
    }

    /// Add a response to the arena.
    ///
    /// - `body` is the type of the response body.
    /// - `headers` is the type of the headers.
    /// - `content_type` is reserved for future mime type support.
    ///
    /// Returns the [`ResponseTypeId`] assigned to the response.
    pub(crate) fn add_response(
        &mut self,
        body: InlineTypeId,
        headers: Option<SimpleRecordReferenceProof>,
        content_type: Option<MimeRangeId>,
    ) -> ResponseTypeId {
        let idx = self.data.len();
        self.data.push(Slot::Response {
            body,
            headers,
            content_type,
        });
        ResponseTypeId(idx as u32)
    }

    /// Start building an enum.
    ///
    /// Parameter `typ` is the type of the values in the enum.
    pub(crate) fn start_enum<'a>(&'a mut self, typ: EnumTy) -> EnumBuilder<'a> {
        EnumBuilder::new(&mut self.data, typ)
    }

    /// Start building a record type.
    ///
    /// Returns a [`FieldBuilder`] rooted at the new record slot.
    pub(crate) fn start_record<'a>(&'a mut self) -> FieldBuilder<'a> {
        FieldBuilder::new_record(self)
    }

    /// Start building a union type.
    ///
    /// Returns a [`FieldBuilder`] rooted at the new union slot.
    pub(crate) fn start_union<'a>(&'a mut self) -> FieldBuilder<'a> {
        FieldBuilder::new_union(self)
    }

    /// Add array to the arena.
    ///
    /// Returns a [`OptionArrayBuilder`] that allows building a nested array.
    pub(crate) fn start_array<'a>(&'a mut self) -> OptionArrayBuilder<'a> {
        OptionArrayBuilder::new_array(&mut self.data, None)
    }

    /// Add option to the arena.
    ///
    /// Returns a [`OptionArrayBuilder`] that allows building a nested option.
    pub(crate) fn start_option<'a>(&'a mut self) -> OptionArrayBuilder<'a> {
        OptionArrayBuilder::new_option(&mut self.data, None)
    }

    /// Constructs arena from builder.
    ///
    /// If any of the references in the builder are not valid, an error is returned. The error
    /// is the name of the reference that couldn't be resolved.
    pub(crate) fn finish(self, table: &SymbolTable) -> Result<TypeArena, StringId> {
        TypeArena::from_builder_data(self.data, table)
    }
}
