use std::any::Any;

use better_api_diagnostic::{Label, Report, Span};
use better_api_syntax::{parse, tokenize};
use indoc::indoc;

use crate::{
    Oracle,
    spec::{
        typ::{EnumTy, InlineTy, PrimitiveTy, RootType, Type},
        value::Value,
    },
};

#[test]
fn lower_primitives() {
    let primitives = [
        ("i32", PrimitiveTy::I32),
        ("i64", PrimitiveTy::I64),
        ("u32", PrimitiveTy::U32),
        ("u64", PrimitiveTy::U64),
        ("f32", PrimitiveTy::F32),
        ("f64", PrimitiveTy::F64),
        ("date", PrimitiveTy::Date),
        ("timestamp", PrimitiveTy::Timestamp),
        ("bool", PrimitiveTy::Bool),
        ("string", PrimitiveTy::String),
        ("file", PrimitiveTy::File),
    ];

    for (raw, expected) in primitives {
        let text = format!("type Foo: {raw}");

        let mut diagnostics = vec![];
        let tokens = tokenize(&text, &mut diagnostics);
        let res = parse(tokens);

        let mut oracle = Oracle::new_raw(&res.root);

        let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
        let id = oracle.lower_type(&typ).unwrap();

        assert_eq!(oracle.reports(), vec![]);

        let actual = oracle.spec_ctx().get_root_type(id);
        assert!(matches!(
            actual,
            RootType::Type(Type::Inline(InlineTy::Primitive(actual_prim)))
                if actual_prim == expected
        ));
    }
}

#[test]
fn lower_reference() {
    let text = indoc! {r#"
        // The type we are validating
        type Foo: Bar

        // Dummy type to have a resolvable reference
        type Bar: string
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);
    oracle.validate_symbols();
    assert!(oracle.reports().is_empty());

    oracle.lower_type_definitions();
    assert!(oracle.reports().is_empty());

    let name_id = oracle.strings.get("Foo").unwrap();
    let type_def = oracle.spec_symbol_table.get(&name_id).unwrap();
    let actual = oracle.spec_ctx().get_root_type(type_def.typ);

    match actual {
        RootType::NamedReference(reference) => {
            assert_eq!(reference.name, "Bar");
            assert!(matches!(
                reference.typ(),
                RootType::Type(Type::Inline(InlineTy::Primitive(PrimitiveTy::String)))
            ));
        }
        _ => panic!("epxected PrimitiveTy::String, got {actual:?}"),
    }
}

#[test]
fn lower_simple_array() {
    let text = indoc! {r#"
        type Foo: [i32]
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
    let id = oracle.lower_type(&typ).unwrap();

    assert_eq!(oracle.reports(), vec![]);

    let arr = match oracle.spec_ctx().get_root_type(id) {
        RootType::Type(Type::Inline(InlineTy::Array(arr))) => arr,
        typ => panic!("expected array, got: {typ:?}"),
    };
    assert!(matches!(arr.typ(), InlineTy::Primitive(PrimitiveTy::I32)));
}

#[test]
fn lower_nested_array() {
    let text = indoc! {r#"
        type Foo: [[i32]]
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
    let id = oracle.lower_type(&typ).unwrap();

    assert_eq!(oracle.reports(), vec![]);

    let arr = match oracle.spec_ctx().get_root_type(id) {
        RootType::Type(Type::Inline(InlineTy::Array(arr))) => arr,
        typ => panic!("expected array, got: {typ:?}"),
    };

    let arr_inner = match arr.typ() {
        InlineTy::Array(arr) => arr,
        typ => panic!("expected array, got: {typ:?}"),
    };
    assert!(matches!(
        arr_inner.typ(),
        InlineTy::Primitive(PrimitiveTy::I32)
    ));
}

#[test]
fn lower_empty_array() {
    let text = indoc! {r#"
        type Foo: []
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
    let id = oracle.lower_type(&typ);
    assert_eq!(id, None);

    assert_eq!(oracle.reports(), vec![]);
}

#[test]
fn lower_invalid_array() {
    let text = indoc! {r#"
        type Foo: [union {}]
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
    let id = oracle.lower_type(&typ);
    assert_eq!(id, None);

    assert_eq!(
            oracle.reports(),
            vec![
                Report::error("inline union not allowed in array".to_string()).add_label(
                    Label::primary("inline union type not allowed".to_string(), Span::new(11, 19))
                ).with_note("help: define a named type first, then reference it\n      example: `type MyUnion: union { ... }`".to_string())
            ]
        );
}

#[test]
fn lower_simple_option() {
    let text = indoc! {r#"
        type Foo: i32?
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
    let id = oracle.lower_type(&typ).unwrap();

    assert_eq!(oracle.reports(), vec![]);

    let opt = match oracle.spec_ctx().get_root_type(id) {
        RootType::Type(Type::Inline(InlineTy::Option(opt))) => opt,
        _ => panic!(),
    };
    assert!(matches!(opt.typ(), InlineTy::Primitive(PrimitiveTy::I32)));
}

#[test]
fn lower_nested_option() {
    // This contains errors reported by parser. We don't care about them here.
    // We are interested that there is a semantic Option<i32> type.
    let text = indoc! {r#"
            type Foo: i32??
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
    let id = oracle.lower_type(&typ).unwrap();

    assert_eq!(oracle.reports(), vec![]);

    let opt = match oracle.spec_ctx().get_root_type(id) {
        RootType::Type(Type::Inline(InlineTy::Option(opt))) => opt,
        _ => panic!(),
    };
    assert!(matches!(opt.typ(), InlineTy::Primitive(PrimitiveTy::I32)));
}

#[test]
fn lower_invalid_option() {
    let text = indoc! {r#"
        type Foo: rec {}?
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    assert!(diagnostics.is_empty());
    assert!(res.reports.is_empty());

    let mut oracle = Oracle::new_raw(&res.root);

    let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
    let id = oracle.lower_type(&typ);
    assert_eq!(id, None);

    assert_eq!(
            oracle.reports(),
            vec![
                Report::error("inline record not allowed in option".to_string())
                    .add_label(Label::primary(
                        "inline record type not allowed".to_string(),
                        Span::new(10, 16)
                    ))
                    .with_note(
                        "help: define a named type first, then reference it\n      example: `type MyRecord: rec { ... }`"
                            .to_string()
                    )
            ]
        );
}

#[test]
fn lower_basic_record() {
    // Test parsing of basic records.
    // We have two records that are the same, but with different field ordering.
    // This test also checks that ordering of parsed is the same, which means we have
    // stable ordering.
    let text = indoc! {r#"
        type Foo: rec {
            foo: string
            bar: [i32]

            @default(69420)
            baz: u32
        }

        type Foo: rec {
            @default(69420)
            baz: u32
            bar: [i32]
            foo: string
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    assert_eq!(res.root.type_definitions().count(), 2);

    for typ in res.root.type_definitions() {
        let typ = typ.typ().unwrap();
        let id = oracle.lower_type(&typ).unwrap();

        assert_eq!(oracle.reports(), vec![]);

        // Check record was inserted
        let rec = match oracle.spec_ctx().get_root_type(id) {
            RootType::Type(Type::Record(rec)) => rec,
            _ => panic!(),
        };

        let fields: Vec<_> = rec.fields().collect();

        // Check field names are correct
        let names: Vec<_> = fields.iter().map(|field| field.name).collect();
        assert_eq!(names, vec!["foo", "bar", "baz"]);

        // Check field @default's are correct
        let defaults: Vec<_> = fields.iter().map(|field| field.default.clone()).collect();
        assert_eq!(defaults.len(), 3);
        assert!(defaults[0].is_none());
        assert!(defaults[1].is_none());
        assert!(matches!(defaults[2], Some(Value::Integer(69420))));
    }
}

#[test]
fn lower_empty_record() {
    let text = indoc! {r#"
        type Foo: rec {}
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
    let id = oracle.lower_type(&typ).unwrap();

    assert_eq!(oracle.reports(), vec![]);

    let rec = match oracle.spec_ctx().get_root_type(id) {
        RootType::Type(Type::Record(rec)) => rec,
        _ => panic!(),
    };

    assert_eq!(rec.fields().count(), 0);
}

#[test]
fn lower_invalid_record() {
    let text = indoc! {r#"
        type Foo: rec {
            invalid: rec {}
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
    let id = oracle.lower_type(&typ).unwrap();

    assert_eq!(
        oracle.reports(),
        vec![
            Report::error("inline record not allowed in record field".to_string()).add_label(
                Label::primary("inline record type not allowed".to_string(), Span::new(29, 35))
            ).with_note("help: define a named type first, then reference it\n      example: `type MyRecord: rec { ... }`".to_string())
        ]
    );

    let rec = match oracle.spec_ctx().get_root_type(id) {
        RootType::Type(Type::Record(rec)) => rec,
        _ => panic!(),
    };

    assert_eq!(rec.fields().count(), 0);
}

#[test]
fn lower_simple_enum() {
    let text = indoc! {r#"
        type Foo: enum (i32) {
            1
            2
            3
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
    let id = oracle.lower_type(&typ).unwrap();

    assert_eq!(oracle.reports(), vec![]);

    // Check enum was inserted and is in source map
    let enum_type = match oracle.spec_ctx().get_root_type(id) {
        RootType::Type(Type::Enum(e)) => e,
        _ => panic!(),
    };

    // Check enum type is i32
    assert!(matches!(enum_type.typ, EnumTy::I32));

    // Check enum members
    let member_vals: Vec<_> = enum_type.members().map(|mem| mem.value).collect();

    assert_eq!(member_vals.len(), 3);
    assert!(matches!(member_vals[0], Value::Integer(1)));
    assert!(matches!(member_vals[1], Value::Integer(2)));
    assert!(matches!(member_vals[2], Value::Integer(3)));
}

#[test]
fn lower_empty_enum() {
    let text = indoc! {r#"
        type Foo: enum (string) {}
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
    let id = oracle.lower_type(&typ).unwrap();

    assert_eq!(oracle.reports(), vec![]);

    let enum_type = match oracle.spec_ctx().get_root_type(id) {
        RootType::Type(Type::Enum(e)) => e,
        _ => panic!(),
    };

    // Check enum members array is empty
    assert_eq!(enum_type.members().count(), 0);
}

#[test]
fn lower_invalid_enum() {
    let text = indoc! {r#"
        type Foo: enum ([i32]) {
            [1, 2, 3]
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
    let id = oracle.lower_type(&typ);
    assert!(id.is_none());

    assert_eq!(
        oracle.reports(),
        vec![
            Report::error("invalid enum type `array`".to_string())
                .add_label(Label::primary(
                    "invalid enum type `array`".to_string(),
                    Span::new(16, 21)
                ))
                .with_note(
                    "help: enum must have a type `i32`, `i64`, `u32`, `u64`, or `string`"
                        .to_string()
                )
        ]
    );
}

#[test]
fn lower_simple_union() {
    let text = indoc! {r#"
        type Foo: union {
            foo: i32

            // Default should be ignored
            @default(69420)
            bar: string
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
    let id = oracle.lower_type(&typ).unwrap();

    assert!(oracle.reports().is_empty());

    // Check union was inserted
    let union = match oracle.spec_ctx().get_root_type(id) {
        RootType::Type(Type::Union(u)) => u,
        _ => panic!(),
    };

    let fields: Vec<_> = union.fields().collect();

    // Check field names are correct
    let names: Vec<_> = fields.iter().map(|field| field.name).collect();
    assert_eq!(names, vec!["foo", "bar"]);

    // Check field types are correct
    assert!(matches!(
        fields[0].typ,
        InlineTy::Primitive(PrimitiveTy::I32)
    ));
    assert!(matches!(
        fields[1].typ,
        InlineTy::Primitive(PrimitiveTy::String)
    ));
}

#[test]
fn lower_empty_union() {
    let text = indoc! {r#"
        type Foo: union {}
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
    let id = oracle.lower_type(&typ).unwrap();

    assert_eq!(oracle.reports(), vec![]);

    let union = match oracle.spec_ctx().get_root_type(id) {
        RootType::Type(Type::Union(u)) => u,
        _ => panic!(),
    };

    assert_eq!(union.fields().count(), 0);
}

#[test]
fn lower_invalid_union() {
    let text = indoc! {r#"
        type Foo: union {
            invalid: rec {}
            invalid2: file
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
    let id = oracle.lower_type(&typ);
    assert!(id.is_none());

    insta::assert_debug_snapshot!(oracle.reports());
}
