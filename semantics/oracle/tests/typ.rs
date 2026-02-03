use better_api_diagnostic::{Label, Report, Span};
use better_api_syntax::{ast, parse, tokenize};
use indoc::indoc;

use crate::{
    Oracle,
    spec::{
        typ::{EnumTy, InlineTy, PrimitiveTy, RootType, SimpleRecordReference, Type},
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

#[test]
fn lower_simple_response() {
    let text = indoc! {r#"
        type Resp: resp {
            body: string
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    assert!(oracle.reports().is_empty());

    let resp = match get_root_type(&oracle, "Resp") {
        RootType::Response(resp) => resp,
        typ => panic!("expected response, got {typ:?}"),
    };

    assert!(matches!(
        resp.body,
        InlineTy::Primitive(PrimitiveTy::String)
    ));
    assert!(resp.headers.is_none());
    assert!(resp.content_type.is_none());
}

#[test]
fn lower_response_with_reference() {
    let text = indoc! {r#"
        type Foo: rec {
            foo: string
        }

        type Bar: Foo

        type Resp: resp {
            body: Bar
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    assert!(oracle.reports().is_empty());

    let resp = match get_root_type(&oracle, "Resp") {
        RootType::Response(resp) => resp,
        typ => panic!("expected response, got {typ:?}"),
    };

    let body_ref = match resp.body {
        InlineTy::NamedReference(reference) => reference,
        typ => panic!("expected named reference, got {typ:?}"),
    };

    assert_eq!(body_ref.name, "Bar");

    let bar_typ = match body_ref.typ() {
        Type::Inline(InlineTy::NamedReference(reference)) => reference,
        typ => panic!("expected reference type, got {typ:?}"),
    };

    assert_eq!(bar_typ.name, "Foo");

    let record = match bar_typ.typ() {
        Type::Record(record) => record,
        typ => panic!("expected record, got {typ:?}"),
    };

    let fields: Vec<_> = record.fields().collect();
    assert_eq!(fields.len(), 1);
    assert_eq!(fields[0].name, "foo");
    assert!(matches!(
        fields[0].typ,
        InlineTy::Primitive(PrimitiveTy::String)
    ));
}

#[test]
fn lower_response_with_inline_body() {
    let text = indoc! {r#"
        type Resp: resp {
            body: rec {
                foo: i32
            }
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    assert!(oracle.reports().is_empty());

    let resp = match get_root_type(&oracle, "Resp") {
        RootType::Response(resp) => resp,
        typ => panic!("expected response, got {typ:?}"),
    };

    let body_ref = match resp.body {
        InlineTy::NamedReference(reference) => reference,
        typ => panic!("expected named reference, got {typ:?}"),
    };

    assert_eq!(body_ref.name, "RespBody");

    let record = match body_ref.typ() {
        Type::Record(record) => record,
        typ => panic!("expected record, got {typ:?}"),
    };

    let fields: Vec<_> = record.fields().collect();
    assert_eq!(fields.len(), 1);
    assert_eq!(fields[0].name, "foo");
    assert!(matches!(
        fields[0].typ,
        InlineTy::Primitive(PrimitiveTy::I32)
    ));
}

#[test]
fn lower_response_invalid_body_response() {
    let text = indoc! {r#"
        type Foo: resp {
            body: string
        }

        type Resp: resp {
            body: Foo
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);

    assert_eq!(
        oracle.reports(),
        vec![
            Report::error("invalid usage of `response` type".to_string())
                .add_label(Label::primary(
                    "invalid usage of `response` type".to_string(),
                    Span::new(65, 68)
                ))
                .add_label(Label::secondary(
                    "`response` type introduced here".to_string(),
                    Span::new(10, 35)
                ))
                .with_note("help: `response` type can't be a child of a type".to_string())
        ]
    );

    let name_id = oracle.strings.get("Resp").unwrap();
    assert!(!oracle.spec_symbol_table.contains_key(&name_id));
}

#[test]
fn lower_response_invalid_body_file() {
    let text = indoc! {r#"
        type Foo: rec {
            foo: file
        }

        type Resp: resp {
            body: Foo
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);

    assert_eq!(oracle.reports().len(), 1);
    assert_eq!(
        oracle.reports(),
        vec![
            Report::error("invalid response body type".to_string())
                .add_label(Label::primary(
                    "content type requires that no `file` is present in body".to_string(),
                    Span::new(61, 64,)
                ))
                .add_label(Label::secondary(
                    "file introduced here".to_string(),
                    Span::new(25, 29)
                ))
                .with_note(
                    "help: `application/json` response must not use `file` in body".to_string()
                )
        ]
    );

    let name_id = oracle.strings.get("Resp").unwrap();
    assert!(!oracle.spec_symbol_table.contains_key(&name_id));
}

#[test]
fn lower_response_with_custom_content_type() {
    let text = indoc! {r#"
        type Resp: resp {
            contentType: "image/png"
            body: file
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    assert!(oracle.reports().is_empty());

    let resp = match get_root_type(&oracle, "Resp") {
        RootType::Response(resp) => resp,
        typ => panic!("expected response, got {typ:?}"),
    };

    let content_types: Vec<_> = resp.content_type.clone().unwrap().collect();
    assert_eq!(content_types, vec!["image/png"]);
    assert!(matches!(resp.body, InlineTy::Primitive(PrimitiveTy::File)));
    assert!(resp.headers.is_none());
}

#[test]
fn lower_response_with_custom_content_type_invalid_body() {
    let text = indoc! {r#"
        type Resp: resp {
            contentType: "image/png"
            body: string
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);

    assert_eq!(oracle.reports().len(), 1);
    assert_eq!(
        oracle.reports(),
        vec![
            Report::error("invalid response body type".to_string())
                .add_label(Label::primary(
                    "content type requires `file` body".to_string(),
                    Span::new(57, 63)
                ))
                .add_label(Label::secondary(
                    "content type defined here".to_string(),
                    Span::new(22, 47)
                ))
                .with_note(
                    "help: none `application/json` responses must use `file` as body".to_string()
                )
        ]
    );

    let name_id = oracle.strings.get("Resp").unwrap();
    assert!(!oracle.spec_symbol_table.contains_key(&name_id));
}

#[test]
fn lower_response_with_headers() {
    let text = indoc! {r#"
        type Foo: rec {
            foo: string
        }

        type Resp: resp {
            headers: Foo
            body: string
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    assert!(oracle.reports().is_empty());

    let resp = match get_root_type(&oracle, "Resp") {
        RootType::Response(resp) => resp,
        typ => panic!("expected response, got {typ:?}"),
    };

    assert!(matches!(
        resp.body,
        InlineTy::Primitive(PrimitiveTy::String)
    ));

    let headers = resp.headers.expect("expected headers");
    assert_eq!(headers.name, "Foo");

    let simple_record = match headers.typ() {
        SimpleRecordReference::SimpleRecord(record) => record,
        typ => panic!("expected simple record, got {typ:?}"),
    };

    let fields: Vec<_> = simple_record.fields().collect();
    assert_eq!(fields.len(), 1);
    assert_eq!(fields[0].name, "foo");
    assert!(matches!(
        fields[0].typ,
        InlineTy::Primitive(PrimitiveTy::String)
    ));
}

#[test]
fn lower_response_invalid_header_type() {
    let text = indoc! {r#"
        type Resp: resp {
            headers: string
            body: string
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);

    assert_eq!(oracle.reports().len(), 1);
    assert_eq!(
        oracle.reports(),
        vec![
            Report::error("invalid header type".to_string())
                .add_label(Label::primary(
                    "expected record, got string".to_string(),
                    Span::new(31, 37)
                ))
                .with_note("help: headers must be a simple record".to_string())
        ]
    );

    let name_id = oracle.strings.get("Resp").unwrap();
    assert!(!oracle.spec_symbol_table.contains_key(&name_id));
}

#[test]
fn lower_response_invalid_header_type_nested_record() {
    let text = indoc! {r#"
        type Foo: rec {
            foo: string
        }

        type Headers: rec {
            bar: Foo
        }

        type Resp: resp {
            headers: Headers
            body: string
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);

    assert_eq!(oracle.reports().len(), 1);
    assert_eq!(
        oracle.reports(),
        vec![
            Report::error("invalid header type".to_string())
                .add_label(Label::primary(
                    "header fields can only be primitive types or option".to_string(),
                    Span::new(102, 109)
                ))
                .add_label(Label::secondary(
                    "non-simple field type here".to_string(),
                    Span::new(64, 67)
                ))
        ]
    );

    let name_id = oracle.strings.get("Resp").unwrap();
    assert!(!oracle.spec_symbol_table.contains_key(&name_id));
}

#[test]
fn lower_response_with_inline_headers() {
    let text = indoc! {r#"
        type Resp: resp {
            headers: rec {
                foo: string
            }

            body: string
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    assert!(oracle.reports().is_empty());

    let resp = match get_root_type(&oracle, "Resp") {
        RootType::Response(resp) => resp,
        typ => panic!("expected response, got {typ:?}"),
    };

    assert!(matches!(
        resp.body,
        InlineTy::Primitive(PrimitiveTy::String)
    ));

    let headers = resp.headers.expect("expected headers");
    assert_eq!(headers.name, "RespHeaders");

    let simple_record = match headers.typ() {
        SimpleRecordReference::SimpleRecord(record) => record,
        typ => panic!("expected simple record, got {typ:?}"),
    };

    let fields: Vec<_> = simple_record.fields().collect();
    assert_eq!(fields.len(), 1);
    assert_eq!(fields[0].name, "foo");
    assert!(matches!(
        fields[0].typ,
        InlineTy::Primitive(PrimitiveTy::String)
    ));
}

#[test]
fn lower_response_headers_with_file() {
    let text = indoc! {r#"
        type Resp: resp {
            headers: rec {
                foo: file
            }

            body: string
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);

    assert_eq!(
        oracle.reports(),
        vec![
            Report::error("invalid headers type".to_string())
                .add_label(Label::primary(
                    "headers type must not contain `file`".to_string(),
                    Span::new(31, 60)
                ))
                .add_label(Label::secondary(
                    "`file` introduced here".to_string(),
                    Span::new(50, 54)
                ))
        ]
    );

    let name_id = oracle.strings.get("Resp").unwrap();
    assert!(!oracle.spec_symbol_table.contains_key(&name_id));
}

#[test]
fn lower_response_headers_with_option() {
    let text = indoc! {r#"
        type Resp: resp {
            headers: rec {
                foo: string?
            }

            body: string
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    assert!(oracle.reports().is_empty());

    let resp = match get_root_type(&oracle, "Resp") {
        RootType::Response(resp) => resp,
        typ => panic!("expected response, got {typ:?}"),
    };

    let headers = resp.headers.expect("expected headers");
    assert_eq!(headers.name, "RespHeaders");

    let simple_record = match headers.typ() {
        SimpleRecordReference::SimpleRecord(record) => record,
        typ => panic!("expected simple record, got {typ:?}"),
    };

    let fields: Vec<_> = simple_record.fields().collect();
    assert_eq!(fields.len(), 1);
    assert_eq!(fields[0].name, "foo");

    let opt = match fields[0].typ.clone() {
        InlineTy::Option(opt) => opt,
        typ => panic!("expected option, got {typ:?}"),
    };
    assert!(matches!(
        opt.typ(),
        InlineTy::Primitive(PrimitiveTy::String)
    ));
}

#[test]
fn lower_response_headers_with_array_invalid() {
    let text = indoc! {r#"
        type Resp: resp {
            headers: rec {
                foo: [string]
            }

            body: string
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);

    assert_eq!(
        oracle.reports(),
        vec![
            Report::error("invalid header type".to_string())
                .add_label(Label::primary(
                    "header fields can only be primitive types or option".to_string(),
                    Span::new(31, 64)
                ))
                .add_label(Label::secondary(
                    "non-simple field type here".to_string(),
                    Span::new(50, 58)
                ))
        ]
    );

    let name_id = oracle.strings.get("Resp").unwrap();
    assert!(!oracle.spec_symbol_table.contains_key(&name_id));
}

#[test]
fn lower_response_missing_body() {
    let text = indoc! {r#"
        type Resp: resp {
            headers: rec {
                foo: string
            }
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);

    assert_eq!(
        oracle.reports(),
        vec![
            Report::error("missing response body".to_string())
                .add_label(Label::primary(
                    "missing response body".to_string(),
                    Span::new(11, 64)
                ))
                .with_note("help: `body` is a required response field".to_string())
        ]
    );

    let name_id = oracle.strings.get("Resp").unwrap();
    assert!(!oracle.spec_symbol_table.contains_key(&name_id));
}

#[test]
fn lower_response_body_file_in_nested_reference() {
    let text = indoc! {r#"
        type Inner: rec {
            foo: file
        }

        type Outer: rec {
            inner: Inner
        }

        type Resp: resp {
            body: Outer
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);

    assert_eq!(
        oracle.reports(),
        vec![
            Report::error("invalid response body type".to_string())
                .add_label(Label::primary(
                    "content type requires that no `file` is present in body".to_string(),
                    Span::new(101, 106)
                ))
                .add_label(Label::secondary(
                    "file introduced here".to_string(),
                    Span::new(27, 31)
                ))
                .with_note(
                    "help: `application/json` response must not use `file` in body".to_string()
                )
        ]
    );

    let name_id = oracle.strings.get("Resp").unwrap();
    assert!(!oracle.spec_symbol_table.contains_key(&name_id));
}

#[test]
fn lower_enum_invalid_member() {
    let text = indoc! {r#"
        type Foo: enum (i32) {
            1
            "two"
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
            Report::error("expected i32, found string".to_string())
                .add_label(Label::primary(
                    "expected i32, found string".to_string(),
                    Span::new(33, 38)
                ))
                .add_label(Label::secondary(
                    "type defined here".to_string(),
                    Span::new(16, 19)
                ))
        ]
    );
}

#[test]
fn lower_response_inline_body_name_collision() {
    let text = indoc! {r#"
        type RespBody: string

        type Resp: resp {
            body: rec {
                foo: i32
            }
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);

    assert_eq!(
        oracle.reports(),
        vec![
            Report::error("name `RespBody` collides with auto generated name".to_string())
                .add_label(Label::primary(
                    "name `RespBody` collides with auto generated name".to_string(),
                    Span::new(5, 13)
                ))
                .add_label(Label::secondary(
                    "autogenerated name for this type".to_string(),
                    Span::new(51, 79)
                ))
        ]
    );

    let name_id = oracle.strings.get("Resp").unwrap();
    assert!(!oracle.spec_symbol_table.contains_key(&name_id));
}

#[test]
fn lower_missing_symbol() {
    let text = indoc! {r#"
        type Foo: Bar
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);

    assert_eq!(
        oracle.reports(),
        vec![
            Report::error("undefined symbol `Bar`".to_string()).add_label(Label::primary(
                "symbol `Bar` is not defined".to_string(),
                Span::new(10, 13)
            ))
        ]
    );

    let name_id = oracle.strings.get("Foo").unwrap();
    assert!(!oracle.spec_symbol_table.contains_key(&name_id));
}

#[test]
fn lower_array_option_response_reference() {
    let text = indoc! {r#"
        type Foo: resp {
            body: string
        }

        type Bar: [[Foo?]?]
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);

    assert_eq!(
        oracle.reports(),
        vec![
            Report::error("invalid usage of `response` type".to_string())
                .add_label(Label::primary(
                    "invalid usage of `response` type".to_string(),
                    Span::new(49, 52)
                ))
                .add_label(Label::secondary(
                    "`response` type introduced here".to_string(),
                    Span::new(10, 35)
                ))
                .with_note("help: `response` type can't be a child of a type".to_string())
        ]
    );

    let name_id = oracle.strings.get("Bar").unwrap();
    assert!(!oracle.spec_symbol_table.contains_key(&name_id));
}

#[test]
fn lower_response_headers_with_enum_reference() {
    let text = indoc! {r#"
        type Foo: enum (string) {
            "foo"
        }

        type Headers: rec {
            header: Foo
        }

        type Resp: resp {
            headers: Headers
            body: string
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let oracle = setup_oracle(&res.root);
    assert!(oracle.reports().is_empty());

    let resp = match get_root_type(&oracle, "Resp") {
        RootType::Response(resp) => resp,
        typ => panic!("expected response, got {typ:?}"),
    };

    let headers = resp.headers.expect("expected headers");
    assert_eq!(headers.name, "Headers");

    let simple_record = match headers.typ() {
        SimpleRecordReference::SimpleRecord(record) => record,
        typ => panic!("expected simple record, got {typ:?}"),
    };

    let fields: Vec<_> = simple_record.fields().collect();
    assert_eq!(fields.len(), 1);
    assert_eq!(fields[0].name, "header");

    let reference = match fields[0].typ.clone() {
        InlineTy::NamedReference(reference) => reference,
        typ => panic!("expected reference, got {typ:?}"),
    };

    assert!(matches!(
        reference.typ(),
        crate::spec::typ::SimpleTy::Enum(_)
    ));
}

fn setup_oracle<'a>(root: &'a ast::Root) -> Oracle<'a> {
    let mut oracle = Oracle::new_raw(root);
    oracle.validate_symbols();
    oracle.lower_type_definitions();
    oracle
}

fn get_root_type<'a>(oracle: &'a Oracle<'_>, name: &str) -> RootType<'a> {
    let name_id = oracle.strings.get(name).unwrap();
    let type_def = oracle.spec_symbol_table.get(&name_id).unwrap();
    oracle.spec_ctx().get_root_type(type_def.typ)
}
