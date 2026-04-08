use super::arena::{InlineTypeData, TypeData};
use super::builder::TypeArenaBuilder;
use super::id::{InlineTypeId, TypeFieldId};
use super::PrimitiveTy;
use crate::spec::Spec;
use crate::text::NameId;

#[test]
fn builds_nested_types() {
    let mut spec = Spec::new_test();
    let mut builder = TypeArenaBuilder::default();

    // Builds:
    // - primitive roots: i32, string, bool
    // - record Root { id: i64, simple_array: [f32], values: [[[string?]]], metadata: [[bool]?] }
    // - union { success: bool, error: string }

    let id_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("id")) };
    let simple_array_name =
        unsafe { NameId::from_string_id(spec.strings.get_or_intern("simple_array")) };
    let values_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("values")) };
    let metadata_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("metadata")) };
    let unused_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("unused")) };
    let success_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("success")) };
    let error_name = unsafe { NameId::from_string_id(spec.strings.get_or_intern("error")) };

    let i32_id = builder.add_primitive(PrimitiveTy::I32);
    let string_id = builder.add_primitive(PrimitiveTy::String);
    let bool_id = builder.add_primitive(PrimitiveTy::Bool);

    let mut root = builder.start_record();
    root.add_primitive(id_name, PrimitiveTy::I64, None, None);

    let (simple_array, simple_array_field_id) = root.start_array(simple_array_name, None, None);
    simple_array.finish_primitive(PrimitiveTy::F32);

    let (mut values_builder, values_field_id) = root.start_array(values_name, None, None);
    values_builder.start_array();
    values_builder.start_array();
    values_builder.start_option();
    let values_id = values_builder.finish_primitive(PrimitiveTy::String);

    {
        let _ = root.start_array(unused_name, None, None);
    }

    let (mut metadata_builder, metadata_field_id) = root.start_array(metadata_name, None, None);
    metadata_builder.start_array();
    metadata_builder.start_option();
    metadata_builder.finish_primitive(PrimitiveTy::Bool);

    let root_id = root.finish();

    {
        let mut dropped_union = builder.start_union();
        dropped_union.add_primitive(unused_name, PrimitiveTy::I32, None, None);
    }

    let mut union_builder = builder.start_union();
    union_builder.add_primitive(success_name, PrimitiveTy::Bool, None, None);
    union_builder.add_primitive(error_name, PrimitiveTy::String, None, None);
    let union_id = union_builder.finish();

    let arena = builder
        .finish(&spec.symbol_table)
        .expect("type arena should build");

    assert!(matches!(
        arena.get_inline_type(i32_id),
        InlineTypeData::Primitive(PrimitiveTy::I32)
    ));
    assert!(matches!(
        arena.get_inline_type(string_id),
        InlineTypeData::Primitive(PrimitiveTy::String)
    ));
    assert!(matches!(
        arena.get_inline_type(bool_id),
        InlineTypeData::Primitive(PrimitiveTy::Bool)
    ));

    let root_record = match arena.get_type(root_id) {
        TypeData::Record(record) => record,
        other => panic!("expected record at root_id, got {other:?}"),
    };

    let cursor = arena.record_cursor(root_record);
    let (id_field, cursor) = arena.next_record_field(cursor).expect("id field");
    assert_eq!(id_field.name, id_name);
    assert_eq!(id_field.id.container_id, root_id);
    assert!(matches!(
        id_field.typ,
        InlineTypeData::Primitive(PrimitiveTy::I64)
    ));
    assert!(id_field.default.is_none());

    let (simple_array_field, cursor) = arena.next_record_field(cursor).expect("simple_array field");
    assert_eq!(simple_array_field.name, simple_array_name);
    let simple_array_inner = match simple_array_field.typ {
        InlineTypeData::Array(array) => arena.get_inline_type(array.inner_id),
        other => panic!("expected array type, got {other:?}"),
    };
    assert!(matches!(
        simple_array_inner,
        InlineTypeData::Primitive(PrimitiveTy::F32)
    ));

    let (values_field, cursor) = arena.next_record_field(cursor).expect("values field");
    assert_eq!(values_field.name, values_name);
    let level_1 = match values_field.typ {
        InlineTypeData::Array(array) => arena.get_inline_type(array.inner_id),
        other => panic!("expected array at level 1, got {other:?}"),
    };
    let level_2 = match level_1 {
        InlineTypeData::Array(array) => arena.get_inline_type(array.inner_id),
        other => panic!("expected array at level 2, got {other:?}"),
    };
    let level_3 = match level_2 {
        InlineTypeData::Array(array) => arena.get_inline_type(array.inner_id),
        other => panic!("expected array at level 3, got {other:?}"),
    };
    let option_inner = match level_3 {
        InlineTypeData::Option(option) => arena.get_inline_type(option.inner_id),
        other => panic!("expected option, got {other:?}"),
    };
    assert!(matches!(
        option_inner,
        InlineTypeData::Primitive(PrimitiveTy::String)
    ));

    let (metadata_field, cursor) = arena.next_record_field(cursor).expect("metadata field");
    assert_eq!(metadata_field.name, metadata_name);
    let metadata_level_1 = match metadata_field.typ {
        InlineTypeData::Array(array) => arena.get_inline_type(array.inner_id),
        other => panic!("expected array at metadata level 1, got {other:?}"),
    };
    let metadata_level_2 = match metadata_level_1 {
        InlineTypeData::Array(array) => arena.get_inline_type(array.inner_id),
        other => panic!("expected array at metadata level 2, got {other:?}"),
    };
    let metadata_inner = match metadata_level_2 {
        InlineTypeData::Option(option) => arena.get_inline_type(option.inner_id),
        other => panic!("expected option in metadata field, got {other:?}"),
    };
    assert!(matches!(
        metadata_inner,
        InlineTypeData::Primitive(PrimitiveTy::Bool)
    ));
    assert!(arena.next_record_field(cursor).is_none());

    let union = match arena.get_type(union_id) {
        TypeData::Union(union) => union,
        other => panic!("expected union at union_id, got {other:?}"),
    };
    let cursor = arena.union_cursor(union);
    let (success_field, cursor) = arena.next_union_field(cursor).expect("success field");
    assert_eq!(success_field.name, success_name);
    assert!(matches!(
        success_field.typ,
        InlineTypeData::Primitive(PrimitiveTy::Bool)
    ));

    let (error_field, cursor) = arena.next_union_field(cursor).expect("error field");
    assert_eq!(error_field.name, error_name);
    assert!(matches!(
        error_field.typ,
        InlineTypeData::Primitive(PrimitiveTy::String)
    ));
    assert!(arena.next_union_field(cursor).is_none());

    assert_eq!(
        arena.get_inline_type(values_id),
        InlineTypeData::Array(super::arena::ArrayData {
            inner_id: InlineTypeId(values_id.0 + 1),
        })
    );

    assert_eq!(
        simple_array_field_id,
        TypeFieldId {
            container_id: root_id,
            slot_idx: 6,
        }
    );
    assert_eq!(
        values_field_id,
        TypeFieldId {
            container_id: root_id,
            slot_idx: 9,
        }
    );
    assert_eq!(
        metadata_field_id,
        TypeFieldId {
            container_id: root_id,
            slot_idx: 15,
        }
    );
}
