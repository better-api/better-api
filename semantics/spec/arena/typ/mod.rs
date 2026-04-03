//! Defines semantic representation of types.
//!
//! ## Querying
//!
//! Types are queried from [`Spec`](crate::spec::Spec) through the spec query
//! context. This module exposes [`Type`], [`RootType`], [`ResponseTy`], and
//! related reference wrappers used during traversal.
//!
//! Within this module, [`Type`] represents the named/composite variants you
//! typically work with. The full representation, including responses and other
//! root-only variants, is [`RootType`].
//!
//! ## Construction
//!
//! Construction is handled by [`Oracle`](crate::Oracle). It builds the internal
//! arenas and performs validation before data is exposed through `SpecContext`.

use crate::spec::arena::typ::id::RootTypeId;
use crate::spec::arena::value::{self, ValueId};
use crate::text::{NameId, StringId};

pub(crate) mod arena;
pub(crate) mod builder;
pub(crate) mod id;

mod slot;

/// Primitive types.
#[derive(Debug, Clone, Copy, PartialEq, derive_more::Display)]
pub enum PrimitiveTy {
    #[display("`i32`")]
    I32,
    #[display("`i64`")]
    I64,
    #[display("`u32`")]
    U32,
    #[display("`u64`")]
    U64,
    #[display("`f32`")]
    F32,
    #[display("`f64`")]
    F64,
    #[display("`date`")]
    Date,
    #[display("`timestamp`")]
    Timestamp,
    #[display("`bool`")]
    Bool,
    #[display("`string`")]
    String,
    #[display("`file`")]
    File,
}

// impl<'a, T> FromSlot<'a> for InlineTy<'a, T> {
//     fn from_slot(ctx: SpecContext<'a>, slot_idx: u32, slot: &Slot) -> Self {
//         match slot {
//             Slot::Primitive(prim) => (*prim).into(),
//             Slot::Array { .. } => Self::Array(Reference {
//                 ctx,
//                 id: slot_idx + 1,
//                 _data: Default::default(),
//             }),
//             Slot::Option { .. } => Self::Option(Reference {
//                 ctx,
//                 id: slot_idx + 1,
//                 _data: Default::default(),
//             }),
//             Slot::Reference(_) => {
//                 let reference = NamedReference::from_slot(ctx, slot_idx, slot);
//                 Self::NamedReference(reference)
//             }
//
//             // The arena and oracle should construct a valid arena
//             _ => unreachable!("invalid conversion of slot {slot_idx}: {slot:?} to InlineTy"),
//         }
//     }
// }

// impl<'a> FromSlot<'a> for SimpleTy<'a> {
//     fn from_slot(ctx: SpecContext<'a>, slot_idx: u32, slot: &Slot) -> Self {
//         match slot {
//             Slot::Primitive(_)
//             | Slot::Array { .. }
//             | Slot::Option { .. }
//             | Slot::Reference { .. } => {
//                 let inline = InlineTy::from_slot(ctx, slot_idx, slot);
//                 Self::Inline(inline)
//             }
//             Slot::Enum { typ, end } => {
//                 let enm = Enum {
//                     typ: *typ,
//                     members: EnumMemberIterator {
//                         ctx,
//                         current: TypeId(slot_idx + 1),
//                         end: *end,
//                     },
//                 };
//                 Self::Enum(enm)
//             }
//
//             // The arena and oracle should construct a valid arena
//             _ => unreachable!("invalid conversion of slot {slot_idx}: {slot:?} to SimpleTy"),
//         }
//     }
// }

// impl<'a> FromSlot<'a> for Type<'a> {
//     fn from_slot(ctx: SpecContext<'a>, slot_idx: u32, slot: &Slot) -> Self {
//         match slot {
//             Slot::Primitive(_)
//             | Slot::Array { .. }
//             | Slot::Option { .. }
//             | Slot::Reference { .. } => {
//                 let inline = InlineTy::from_slot(ctx, slot_idx, slot);
//                 Self::Inline(inline)
//             }
//             Slot::Enum { typ, end } => {
//                 let enm = Enum {
//                     typ: *typ,
//                     members: EnumMemberIterator {
//                         ctx,
//                         current: TypeId(slot_idx + 1),
//                         end: *end,
//                     },
//                 };
//                 Self::Enum(enm)
//             }
//             Slot::Record { end } => Type::Record(Record {
//                 fields: TypeFieldIterator {
//                     ctx,
//                     current: slot_idx + 1,
//                     end: end.0,
//                     id: TypeId(slot_idx),
//                     _data: Default::default(),
//                 },
//             }),
//             Slot::Union { end } => Type::Union(Union {
//                 fields: TypeFieldIterator {
//                     ctx,
//                     current: slot_idx + 1,
//                     end: end.0,
//                     id: TypeId(slot_idx),
//                     _data: Default::default(),
//                 },
//             }),
//
//             // The arena and oracle should construct a valid arena
//             _ => unreachable!("invalid conversion of slot {slot_idx}: {slot:?} to Type"),
//         }
//     }
// }

// impl<'a, T> FromSlot<'a> for NamedReference<'a, T> {
//     fn from_slot(ctx: SpecContext<'a>, slot_idx: u32, slot: &Slot) -> Self {
//         match slot {
//             Slot::Reference(name_id) => {
//                 let name = ctx.strings.resolve(*name_id);
//
//                 let type_def = ctx
//                     .symbol_table
//                     .get(name_id)
//                     .expect("invalid type arena: missing reference in symbol table");
//
//                 NamedReference {
//                     name,
//                     id: type_def.typ.0,
//                     ctx,
//                     _data: Default::default(),
//                 }
//             }
//
//             // The arena and oracle should construct a valid arena
//             _ => unreachable!("invalid conversion of slot {slot_idx}: {slot:?} to NamedReference"),
//         }
//     }
// }

// impl<'a> FromSlot<'a> for SimpleRecord<'a> {
//     fn from_slot(ctx: SpecContext<'a>, slot_idx: u32, slot: &Slot) -> Self {
//         match slot {
//             Slot::Record { end } => Self {
//                 fields: TypeFieldIterator {
//                     ctx,
//                     current: slot_idx + 1,
//                     end: end.0,
//                     id: TypeId(slot_idx),
//                     _data: Default::default(),
//                 },
//             },
//
//             // The arena and oracle should construct a valid arena
//             _ => unreachable!("invalid conversion of slot {slot_idx}: {slot:?} to SimpleRecord"),
//         }
//     }
// }

/// Valid enum types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum EnumTy {
    #[display("`i32`")]
    I32,
    #[display("`i64`")]
    I64,
    #[display("`u32`")]
    U32,
    #[display("`u64`")]
    U64,
    #[display("`string`")]
    String,
}

// impl<'a> FromSlot<'a> for ResponseTy<'a> {
//     fn from_slot(ctx: SpecContext<'a>, slot_idx: u32, slot: &Slot) -> Self {
//         let Slot::Response {
//             body,
//             headers,
//             content_type,
//         } = slot
//         else {
//             unreachable!("invalid ResponseId {slot_idx} that doesn't point to response");
//         };
//
//         let body = ctx.get_inline_type(*body);
//         let headers = headers.map(|id| ctx.get_simple_record_reference(id));
//
//         let val_ctx: ValueContext = ctx.into();
//         let content_type = content_type.map(|id| val_ctx.get_mime_types(id));
//
//         ResponseTy {
//             body,
//             headers,
//             content_type,
//         }
//     }
// }

// impl<'a> FromSlot<'a> for RootType<'a> {
//     fn from_slot(ctx: SpecContext<'a>, slot_idx: u32, slot: &Slot) -> Self {
//         match slot {
//             Slot::Response { .. } => {
//                 let resp = ResponseTy::from_slot(ctx, slot_idx, slot);
//                 Self::Response(resp)
//             }
//             Slot::Reference(_) => {
//                 let reference = NamedReference::from_slot(ctx, slot_idx, slot);
//                 Self::NamedReference(reference)
//             }
//             _ => {
//                 let typ = Type::from_slot(ctx, slot_idx, slot);
//                 Self::Type(typ)
//             }
//         }
//     }
// }

// impl<'a> FromSlot<'a> for ResponseReference<'a> {
//     fn from_slot(ctx: SpecContext<'a>, slot_idx: u32, slot: &Slot) -> Self {
//         match slot {
//             Slot::Response { .. } => {
//                 let resp = ResponseTy::from_slot(ctx, slot_idx, slot);
//                 Self::Response(resp)
//             }
//             Slot::Reference(_) => {
//                 let reference = NamedReference::from_slot(ctx, slot_idx, slot);
//                 Self::NamedReference(reference)
//             }
//
//             // The arena and oracle should construct a valid arena
//             _ => {
//                 unreachable!("invalid conversion of slot {slot_idx}: {slot:?} to ResponseReference")
//             }
//         }
//     }
// }
//

// impl<'a> FromSlot<'a> for SimpleRecordReference<'a> {
//     fn from_slot(ctx: SpecContext<'a>, slot_idx: u32, slot: &Slot) -> Self {
//         match slot {
//             Slot::Record { .. } => {
//                 let simple_rec = SimpleRecord::from_slot(ctx, slot_idx, slot);
//                 Self::SimpleRecord(simple_rec)
//             }
//             Slot::Reference(_) => {
//                 let reference = NamedReference::from_slot(ctx, slot_idx, slot);
//                 Self::NamedReference(reference)
//             }
//
//             // The arena and oracle should construct a valid arena
//             _ => {
//                 unreachable!(
//                     "invalid conversion of slot {slot_idx}: {slot:?} to SimpleRecordReference"
//                 )
//             }
//         }
//     }
// }

/// Type definition.
///
/// Stores the doc comment for a named type definition.
#[derive(Debug, Clone)]
pub(crate) struct TypeDefData {
    #[expect(
        dead_code,
        reason = "Stored for future semantic queries and diagnostics"
    )]
    pub docs: Option<StringId>,
    pub typ: RootTypeId,
    #[expect(
        dead_code,
        reason = "Stored for future semantic queries and diagnostics"
    )]
    pub name: StringId,
}

// impl From<SimpleRecordReferenceId> for RootTypeId {
//     fn from(value: SimpleRecordReferenceId) -> Self {
//         Self(value.0)
//     }
// }

//
// impl SimpleRecordReferenceId {
//     /// Treat a [`RootTypeId`] as a [`SimpleRecordReferenceId`] without validation.
//     ///
//     /// ## Safety
//     ///
//     /// The caller must ensure the id refers to a named reference that terminates
//     /// with a [`SimpleRecord`]. That is, id must be a valid
//     /// `NamedReference<SimpleRecordReference>`
//     pub(crate) unsafe fn new_unchecked(id: RootTypeId) -> Self {
//         Self(id.0)
//     }
// }

// impl ResponseReferenceId {
//     /// Treat a [`RootTypeId`] as a [`ResponseReferenceId`] without validation.
//     ///
//     /// ## Safety
//     ///
//     /// The caller must ensure the id refers to a named reference that terminates
//     /// with a [`ResponseTy`]. That is, id must be a valid `NamedReference<ResponseReference>`.
//     pub(crate) unsafe fn new_unchecked(id: RootTypeId) -> Self {
//         Self(id.0)
//     }
// }
//

// impl<'a> SpecContext<'a> {
//     /// Helper method for resolving type ids
//     fn resolve_from_slot<T: FromSlot<'a>>(&self, id: u32) -> T {
//         let slot = &self.types.data[id as usize];
//         T::from_slot(*self, id, slot)
//     }
//
//     /// Get [`Type`] by id.
//     #[cfg_attr(
//         not(test),
//         expect(dead_code, reason = "Should be used later on by semantic query APIs")
//     )]
//     pub(crate) fn get_type(&self, id: TypeId) -> Type<'a> {
//         self.resolve_from_slot(id.0)
//     }
//
//     /// Get [`InlineTy`] type by id.
//     pub(crate) fn get_inline_type(&self, id: InlineTyId) -> InlineTy<'a, Type<'a>> {
//         self.resolve_from_slot(id.0)
//     }
//
//     /// Get [`SimpleRecordReference`] type by id.
//     pub(crate) fn get_simple_record_reference(
//         &self,
//         id: SimpleRecordReferenceId,
//     ) -> NamedReference<'a, SimpleRecordReference<'a>> {
//         self.resolve_from_slot(id.0)
//     }
//
//     /// Get [`ResponseReference`] type by id.
//     pub(crate) fn get_response_reference(
//         &self,
//         id: ResponseReferenceId,
//     ) -> NamedReference<'a, ResponseReference<'a>> {
//         self.resolve_from_slot(id.0)
//     }
//
//     /// Get [`ResponseTy`] type by id.
//     #[expect(dead_code, reason = "Should be used later on by semantic query APIs")]
//     pub(crate) fn get_response_type(&self, id: ResponseTyId) -> ResponseTy<'a> {
//         self.resolve_from_slot(id.0)
//     }
//
//     /// Get [`RootType`] by id.
//     #[cfg_attr(
//         not(test),
//         expect(dead_code, reason = "Should be used later on by semantic query APIs")
//     )]
//     pub(crate) fn get_root_type(&self, id: RootTypeId) -> RootType<'a> {
//         self.resolve_from_slot(id.0)
//     }
//
//     /// Auxiliary function for getting inner type field representation by id
//     fn get_type_field<T>(&self, id: TypeFieldId) -> TypeFieldInner<'a, T> {
//         let field_slot = &self.types.data[id.slot_idx as usize];
//         let typ_slot = &self.types.data[id.slot_idx as usize + 1];
//
//         let (name_id, default_id, docs_id) = match &field_slot {
//             Slot::TypeField {
//                 name,
//                 default,
//                 docs,
//             } => (*name, *default, *docs),
//             val => unreachable!("invalid type field in arena for id {id:?}: {val:?}"),
//         };
//
//         let name = self.strings.resolve_name(name_id);
//
//         let val_ctx: ValueContext = (*self).into();
//         let default = default_id.map(|id| val_ctx.get_value(id));
//         let docs = docs_id.map(|id| self.strings.resolve(id));
//
//         let typ = InlineTy::from_slot(*self, id.slot_idx + 1, typ_slot);
//
//         TypeFieldInner {
//             id,
//             name,
//             default,
//             docs,
//             typ,
//         }
//     }
// }

#[cfg(test)]
mod test {
    use super::{
        EnumTy, PrimitiveTy, RootRef, RootType, SimpleTy, Slot, Type, TypeFieldId, TypeId,
    };
    use crate::spec::typ::{InlineTyId, SimpleRecordReference, SimpleRecordReferenceId};
    use crate::spec::value::{PrimitiveValue, Value};
    use crate::spec::{
        Spec,
        typ::{InlineTy, TypeDefData, TypeRef},
    };
    use crate::text::NameId;

    #[test]
    fn builds_nested_types() {
        let mut spec = Spec::new_test();

        // Insert some strings
        let id_str = spec.strings.get_or_intern("id");
        let id_name = unsafe { NameId::from_string_id(id_str) };
        let simple_array_str = spec.strings.get_or_intern("simple_array");
        let simple_array_name = unsafe { NameId::from_string_id(simple_array_str) };
        let values_str = spec.strings.get_or_intern("values");
        let values_name = unsafe { NameId::from_string_id(values_str) };
        let metadata_str = spec.strings.get_or_intern("metadata");
        let metadata_name = unsafe { NameId::from_string_id(metadata_str) };
        let unused_str = spec.strings.get_or_intern("unused");
        let unused_name = unsafe { NameId::from_string_id(unused_str) };
        let success_str = spec.strings.get_or_intern("success");
        let success_name = unsafe { NameId::from_string_id(success_str) };
        let error_str = spec.strings.get_or_intern("error");
        let error_name = unsafe { NameId::from_string_id(error_str) };

        // Add some primitive types
        let i32_id = spec.types.add_primitive(PrimitiveTy::I32);
        let string_id = spec.types.add_primitive(PrimitiveTy::String);
        let bool_id = spec.types.add_primitive(PrimitiveTy::Bool);

        // Build a complex nested type structure:
        // record Root {
        //   id: i64,
        //   simple_array: [f32],
        //   values: [[[string?]]],
        //   metadata: [[bool]?]
        // }
        let mut root = spec.types.start_record();
        root.add_primitive(id_name, PrimitiveTy::I64, None, None);

        let (simple_array, simple_array_field_id) = root.start_array(simple_array_name, None, None);
        simple_array.finish_primitive(PrimitiveTy::F32);

        // Build nested array type: [[[string?]]]
        let (mut values_builder, values_field_id) = root.start_array(values_name, None, None);
        values_builder.start_array();
        values_builder.start_array();
        values_builder.start_option();
        let values_id = values_builder.finish_primitive(PrimitiveTy::String);

        // Test dropped builder (should not appear in root)
        {
            let _ = root.start_array(unused_name, None, None);
            // Dropped without finish
        }

        // Build nested array with option: [[bool]?]
        let (mut metadata_builder, metadata_field_id) = root.start_array(metadata_name, None, None);
        metadata_builder.start_array();
        metadata_builder.start_option();
        metadata_builder.finish_primitive(PrimitiveTy::Bool);

        let root_id = root.finish();

        // Test dropped builder (should not appear in arena)
        {
            let mut dropped_union = spec.types.start_union();
            dropped_union.add_primitive(unused_name, PrimitiveTy::I32, None, None);
            // Dropped without finish
        }

        // Build a union type separately
        let mut union_builder = spec.types.start_union();
        union_builder.add_primitive(success_name, PrimitiveTy::Bool, None, None);
        union_builder.add_primitive(error_name, PrimitiveTy::String, None, None);
        let union_id = union_builder.finish();

        // Verify the arena structure
        let expected_slots = vec![
            Slot::Primitive(PrimitiveTy::I32),
            Slot::Primitive(PrimitiveTy::String),
            Slot::Primitive(PrimitiveTy::Bool),
            Slot::Record { end: TypeId(20) },
            Slot::TypeField {
                name: id_name,
                default: None,
                docs: None,
            },
            Slot::Primitive(PrimitiveTy::I64),
            Slot::TypeField {
                name: simple_array_name,
                default: None,
                docs: None,
            },
            Slot::Array { end: TypeId(9) },
            Slot::Primitive(PrimitiveTy::F32),
            Slot::TypeField {
                name: values_name,
                default: None,
                docs: None,
            },
            Slot::Array { end: TypeId(15) },
            Slot::Array { end: TypeId(15) },
            Slot::Array { end: TypeId(15) },
            Slot::Option { end: TypeId(15) },
            Slot::Primitive(PrimitiveTy::String),
            Slot::TypeField {
                name: metadata_name,
                default: None,
                docs: None,
            },
            Slot::Array { end: TypeId(20) },
            Slot::Array { end: TypeId(20) },
            Slot::Option { end: TypeId(20) },
            Slot::Primitive(PrimitiveTy::Bool),
            Slot::Union { end: TypeId(25) },
            Slot::TypeField {
                name: success_name,
                default: None,
                docs: None,
            },
            Slot::Primitive(PrimitiveTy::Bool),
            Slot::TypeField {
                name: error_name,
                default: None,
                docs: None,
            },
            Slot::Primitive(PrimitiveTy::String),
        ];

        assert!(spec.types.data.len() == expected_slots.len());
        for (idx, expected) in expected_slots.iter().enumerate() {
            assert!(
                &spec.types.data[idx] == expected,
                "slot mismatch at index {idx}"
            );
        }

        // Test getting primitive types
        assert!(matches!(
            spec.ctx().get_type(i32_id),
            Type::Inline(InlineTy::Primitive(PrimitiveTy::I32))
        ));
        assert!(matches!(
            spec.ctx().get_type(string_id),
            Type::Inline(InlineTy::Primitive(PrimitiveTy::String))
        ));
        assert!(matches!(
            spec.ctx().get_type(bool_id),
            Type::Inline(InlineTy::Primitive(PrimitiveTy::Bool))
        ));

        // Test getting the root record
        let root_type = spec.ctx().get_type(root_id);
        let root_record = match root_type {
            Type::Record(record) => record,
            other => panic!("expected record at root_id, got {other:?}"),
        };

        let mut root_record_fields = root_record.fields();

        // Check id field
        let id_field = root_record_fields.next().expect("id field");
        assert_eq!(id_field.name.as_str(), "id");
        assert!(matches!(
            id_field.typ,
            InlineTy::Primitive(PrimitiveTy::I64)
        ));
        assert!(id_field.default.is_none());

        // Check simple_array field
        let simple_array_field = root_record_fields.next().expect("simple_array field");
        assert_eq!(simple_array_field.name.as_str(), "simple_array");
        match simple_array_field.typ {
            InlineTy::Array(ref inner) => {
                assert!(matches!(inner.typ(), InlineTy::Primitive(PrimitiveTy::F32)));
            }
            other => panic!("expected array type, got {other:?}"),
        }

        // Check values field with nested arrays
        let values_field = root_record_fields.next().expect("values field");
        assert_eq!(values_field.name.as_str(), "values");

        // Navigate through [[[string?]]]
        let level1 = match values_field.typ {
            InlineTy::Array(ref inner) => inner.typ(),
            other => panic!("expected array at level 1, got {other:?}"),
        };

        let level2 = match level1 {
            InlineTy::Array(ref inner) => inner.typ(),
            other => panic!("expected array at level 2, got {other:?}"),
        };

        let level3 = match level2 {
            InlineTy::Array(ref inner) => inner.typ(),
            other => panic!("expected array at level 3, got {other:?}"),
        };

        let option_inner = match level3 {
            InlineTy::Option(ref inner) => inner.typ(),
            other => panic!("expected option, got {other:?}"),
        };

        assert!(matches!(
            option_inner,
            InlineTy::Primitive(PrimitiveTy::String)
        ));

        // Check metadata field
        let metadata_field = root_record_fields.next().expect("metadata field");
        assert_eq!(metadata_field.name.as_str(), "metadata");

        assert!(root_record_fields.next().is_none());

        // Test getting the union directly
        let union_type = spec.ctx().get_type(union_id);
        let mut union_fields = match union_type {
            Type::Union(u) => u.fields(),
            other => panic!("expected union at union_id, got {other:?}"),
        };

        let success_field = union_fields.next().expect("success field");
        assert_eq!(success_field.name.as_str(), "success");
        assert!(matches!(
            success_field.typ,
            InlineTy::Primitive(PrimitiveTy::Bool)
        ));

        let error_field = union_fields.next().expect("error field");
        assert_eq!(error_field.name.as_str(), "error");
        assert!(matches!(
            error_field.typ,
            InlineTy::Primitive(PrimitiveTy::String)
        ));

        assert!(union_fields.next().is_none());

        // Test getting values array type directly
        let values_type = spec.ctx().get_type(values_id.container_id);
        match values_type {
            Type::Inline(InlineTy::Array(ref inner)) => {
                let level1 = inner.typ();
                match level1 {
                    InlineTy::Array(ref inner2) => {
                        let level2 = inner2.typ();
                        match level2 {
                            InlineTy::Array(ref inner3) => {
                                let level3 = inner3.typ();
                                match level3 {
                                    InlineTy::Option(ref inner4) => {
                                        assert!(matches!(
                                            inner4.typ(),
                                            InlineTy::Primitive(PrimitiveTy::String)
                                        ));
                                    }
                                    other => {
                                        panic!("expected option at deepest level, got {other:?}")
                                    }
                                }
                            }
                            other => panic!("expected array at level 3, got {other:?}"),
                        }
                    }
                    other => panic!("expected array at level 2, got {other:?}"),
                }
            }
            other => panic!("expected array at values_id, got {other:?}"),
        }

        // Check field ids
        assert!(
            simple_array_field_id
                == TypeFieldId {
                    container_id: root_id,
                    slot_idx: 6,
                }
        );

        assert!(
            values_field_id
                == TypeFieldId {
                    container_id: root_id,
                    slot_idx: 9,
                }
        );

        assert!(
            metadata_field_id
                == TypeFieldId {
                    container_id: root_id,
                    slot_idx: 15,
                }
        );
    }

    #[test]
    fn named_refs() {
        let mut spec = Spec::new_test();

        // Build spec like this:
        // type Foo: string
        // type Bar: rec {
        //   name: Foo
        // }

        let foo_name = spec.strings.get_or_intern("Foo");
        let name_field_str = spec.strings.get_or_intern("name");
        let name_field_name = unsafe { NameId::from_string_id(name_field_str) };

        let foo_typ = spec.types.add_primitive(PrimitiveTy::String);
        spec.symbol_table.insert(
            foo_name,
            TypeDefData {
                docs: None,
                typ: foo_typ.into(),
                name: foo_name,
            },
        );

        let mut bar_builder = spec.types.start_record();
        bar_builder.add_reference(name_field_name, TypeRef(foo_name), None, None);
        let bar_typ = bar_builder.finish();

        // Get Bar type and verify
        let record = match spec.ctx().get_type(bar_typ) {
            Type::Record(record) => record,
            _ => panic!("expected record type for Bar"),
        };

        // get name field
        let mut fields = record.fields();
        let name_field = fields.next().expect("name field should exist");

        // Check no more fields
        assert!(fields.next().is_none());

        match name_field.typ {
            InlineTy::NamedReference(reference) => {
                assert_eq!(reference.name, "Foo");
                assert!(matches!(
                    reference.typ(),
                    Type::Inline(InlineTy::Primitive(PrimitiveTy::String))
                ))
            }
            _ => panic!("expected named reference for name field"),
        }
    }

    #[test]
    fn simple_record_with_enum_option() {
        let mut spec = Spec::new_test();

        let record_name = spec.strings.get_or_intern("SimpleRec");
        let status_name = spec.strings.get_or_intern("Status");
        let open_str = spec.strings.get_or_intern("open");
        let closed_str = spec.strings.get_or_intern("closed");
        let id_str = spec.strings.get_or_intern("id");
        let id_name = unsafe { NameId::from_string_id(id_str) };
        let title_str = spec.strings.get_or_intern("title");
        let title_name = unsafe { NameId::from_string_id(title_str) };
        let status_str = spec.strings.get_or_intern("status");
        let status_name_id = unsafe { NameId::from_string_id(status_str) };

        let open_value = spec.values.add_primitive(PrimitiveValue::String(open_str));
        let closed_value = spec
            .values
            .add_primitive(PrimitiveValue::String(closed_str));

        let mut enum_builder = spec.types.start_enum(EnumTy::String);
        enum_builder.add_member(open_value, None);
        enum_builder.add_member(closed_value, None);
        let enum_id = enum_builder.finish();

        spec.symbol_table.insert(
            status_name,
            TypeDefData {
                docs: None,
                typ: enum_id.into(),
                name: status_name,
            },
        );

        let mut record_builder = spec.types.start_record();
        record_builder.add_primitive(id_name, PrimitiveTy::I64, None, None);
        record_builder.add_primitive(title_name, PrimitiveTy::String, None, None);
        let (status_builder, _) = record_builder.start_option(status_name_id, None, None);
        status_builder.finish_reference(TypeRef(status_name));
        let record_id = record_builder.finish();

        spec.symbol_table.insert(
            record_name,
            TypeDefData {
                docs: None,
                typ: record_id.into(),
                name: record_name,
            },
        );

        let ref_id = unsafe {
            SimpleRecordReferenceId::new_unchecked(spec.types.add_reference(RootRef(record_name)))
        };

        let simple_rec_ref = spec.ctx().get_simple_record_reference(ref_id);
        let record = match simple_rec_ref.typ() {
            SimpleRecordReference::SimpleRecord(record) => record,
            SimpleRecordReference::NamedReference(_) => {
                panic!("expected simple record, got named reference")
            }
        };

        let mut fields = record.fields();

        let id_field = fields.next().expect("id field");
        assert_eq!(id_field.name.as_str(), "id");
        assert!(matches!(
            id_field.typ,
            InlineTy::Primitive(PrimitiveTy::I64)
        ));

        let title_field = fields.next().expect("title field");
        assert_eq!(title_field.name.as_str(), "title");
        assert!(matches!(
            title_field.typ,
            InlineTy::Primitive(PrimitiveTy::String)
        ));

        let status_field = fields.next().expect("status field");
        assert_eq!(status_field.name.as_str(), "status");
        let option_inner = match status_field.typ {
            InlineTy::Option(ref inner) => inner.typ(),
            other => panic!("expected option for status field, got {other:?}"),
        };

        let status_enum = match option_inner {
            InlineTy::NamedReference(reference) => {
                assert_eq!(reference.name, "Status");
                match reference.typ() {
                    SimpleTy::Enum(enm) => enm,
                    other => panic!("expected enum for status reference, got {other:?}"),
                }
            }
            other => panic!("expected named reference for status field, got {other:?}"),
        };

        let mut members = status_enum.members();
        let open_member = members.next().expect("open enum member");
        assert!(matches!(open_member.value, Value::String("open")));
        let closed_member = members.next().expect("closed enum member");
        assert!(matches!(closed_member.value, Value::String("closed")));
        assert!(members.next().is_none());

        assert!(fields.next().is_none());
    }

    #[test]
    fn root_reference_response_enum_body() {
        let mut spec = Spec::new_test();

        let enum_name = spec.strings.get_or_intern("Enum");
        let response_name = spec.strings.get_or_intern("StatusResponse");
        let ok_str = spec.strings.get_or_intern("ok");
        let error_str = spec.strings.get_or_intern("error");

        let ok_value = spec.values.add_primitive(PrimitiveValue::String(ok_str));
        let error_value = spec.values.add_primitive(PrimitiveValue::String(error_str));

        let mut enum_builder = spec.types.start_enum(EnumTy::String);
        enum_builder.add_member(ok_value, None);
        enum_builder.add_member(error_value, None);
        let enum_id = enum_builder.finish();

        spec.symbol_table.insert(
            enum_name,
            TypeDefData {
                docs: None,
                typ: enum_id.into(),
                name: enum_name,
            },
        );
        let enum_ref =
            unsafe { InlineTyId::new_unchecked(spec.types.add_reference(RootRef(enum_name))) };

        let response_id = spec.types.add_response(enum_ref, None, None);

        spec.symbol_table.insert(
            response_name,
            TypeDefData {
                docs: None,
                typ: response_id.into(),
                name: response_name,
            },
        );

        let response_ref_id = spec.types.add_reference(RootRef(response_name));
        let response_root = spec.ctx().get_root_type(response_ref_id);

        let response = match response_root {
            RootType::NamedReference(reference) => match reference.typ() {
                RootType::Response(response) => response,
                other => panic!("expected response type, got {other:?}"),
            },

            other => panic!("expected reference to response, got {other:?}"),
        };

        assert!(response.headers.is_none());
        assert!(response.content_type.is_none());

        let status_enum = match response.body {
            InlineTy::NamedReference(reference) => match reference.typ() {
                Type::Enum(enm) => enm,
                other => panic!("expected enum response body, got {other:?}"),
            },
            other => panic!("expected named reference response body, got {other:?}"),
        };
        assert_eq!(status_enum.typ, EnumTy::String);

        let mut members = status_enum.members();
        let ok_member = members.next().expect("ok enum member");
        assert!(matches!(ok_member.value, Value::String("ok")));
        let error_member = members.next().expect("error enum member");
        assert!(matches!(error_member.value, Value::String("error")));
        assert!(members.next().is_none());
    }
}
