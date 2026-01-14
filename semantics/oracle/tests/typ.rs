use better_api_diagnostic::{Label, Report, Span};
use better_api_syntax::{parse, tokenize};
use indoc::indoc;

use crate::{Oracle, typ::Type, value::Value};

#[test]
fn parse_primitives() {
    let primitives = [
        ("i32", Type::I32),
        ("i64", Type::I64),
        ("u32", Type::U32),
        ("u64", Type::U64),
        ("f32", Type::F32),
        ("f64", Type::F64),
        ("date", Type::Date),
        ("timestamp", Type::Timestamp),
        ("bool", Type::Bool),
        ("string", Type::String),
        ("file", Type::File),
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
        assert_eq!(oracle.types.get(id), expected);

        oracle.source_map.get_type(id);
    }
}

#[test]
fn parse_primitive_reference() {
    let text = indoc! {r#"
            type Foo: Bar
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
    let id = oracle.lower_type(&typ).unwrap();

    let name = oracle.strings.insert("Bar");
    assert_eq!(oracle.reports(), vec![]);
    assert_eq!(oracle.types.get(id), Type::Reference(name));

    oracle.source_map.get_type(id);
}

#[test]
fn parse_simple_array() {
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

    let arr = match oracle.types.get(id) {
        Type::Array(arr) => arr,
        _ => panic!(),
    };
    assert_eq!(arr.typ(), Type::I32);

    oracle.source_map.get_type(id);
    oracle.source_map.get_type(arr.id);
}

#[test]
fn parse_nested_array() {
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

    let arr = match oracle.types.get(id) {
        Type::Array(arr) => arr,
        _ => panic!(),
    };
    let arr_inner = match arr.typ() {
        Type::Array(arr) => arr,
        _ => panic!(),
    };
    assert_eq!(arr_inner.typ(), Type::I32);

    oracle.source_map.get_type(id);
    oracle.source_map.get_type(arr.id);
    oracle.source_map.get_type(arr_inner.id);
}

#[test]
fn parse_empty_array() {
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
fn parse_invalid_array() {
    let text = indoc! {r#"
            type Foo: [union (string) {}]
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
                    Label::primary("inline union type not allowed".to_string(), Span::new(11, 28))
                ).with_note("help: define a named type first, then reference it\n      example: `type MyUnion: union (\"discriminator\") { ... }`".to_string())
            ]
        );
}

#[test]
fn parse_simple_option() {
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

    let opt = match oracle.types.get(id) {
        Type::Option(opt) => opt,
        _ => panic!(),
    };
    assert_eq!(opt.typ(), Type::I32);

    oracle.source_map.get_type(id);
    oracle.source_map.get_type(opt.id);
}

#[test]
fn parse_nested_option() {
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

    let opt = match oracle.types.get(id) {
        Type::Option(opt) => opt,
        _ => panic!(),
    };
    assert_eq!(opt.typ(), Type::I32);

    oracle.source_map.get_type(id);
    oracle.source_map.get_type(opt.id);
}

#[test]
fn parse_invalid_option() {
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
fn parse_simple_record() {
    // Test parsing of simple records.
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

        // Check record was inserted and is in source map
        let rec = match oracle.types.get(id) {
            Type::Record(rec) => rec,
            _ => panic!(),
        };
        oracle.source_map.get_type(id);

        let fields: Vec<_> = rec.collect();

        // Check field names are correct
        let names: Vec<_> = fields
            .iter()
            .map(|field| oracle.strings.get(field.name))
            .collect();
        assert_eq!(names, vec!["foo", "bar", "baz"]);

        // Check field @default's are correct
        let defaults: Vec<_> = fields
            .iter()
            .map(|field| field.default.map(|id| oracle.values.get(id)))
            .collect();
        assert_eq!(defaults.len(), 3);
        assert!(defaults[0].is_none());
        assert!(defaults[1].is_none());
        assert!(matches!(defaults[2], Some(Value::Integer(69420))));

        // Check default values are in source map
        for default_id in fields.iter().filter_map(|field| field.default) {
            oracle.source_map.get_value(default_id);
        }

        // Check fields and field types are in source map
        for field in &fields {
            oracle.source_map.get_type(field.id.type_id());
        }
    }
}

#[test]
fn parse_empty_record() {
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

    let rec = match oracle.types.get(id) {
        Type::Record(rec) => rec,
        _ => panic!(),
    };
    oracle.source_map.get_type(id);

    assert_eq!(rec.count(), 0);
}

#[test]
fn parse_invalid_record() {
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

    let rec = match oracle.types.get(id) {
        Type::Record(rec) => rec,
        _ => panic!(),
    };
    oracle.source_map.get_type(id);

    assert_eq!(rec.count(), 0);
}

#[test]
fn parse_simple_enum() {
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
    let enum_type = match oracle.types.get(id) {
        Type::Enum(e) => e,
        _ => panic!(),
    };
    oracle.source_map.get_type(id);

    // Check enum type is i32
    let enum_type_inner = enum_type.typ.unwrap();
    assert_eq!(enum_type_inner.typ(), Type::I32);

    // Check enum type is inside of source map
    oracle.source_map.get_type(enum_type_inner.id);

    // Check enum members
    let members: Vec<_> = match oracle.values.get(enum_type.values) {
        Value::Array(arr) => arr.items().collect(),
        _ => panic!(),
    };

    let member_vals: Vec<_> = members.iter().map(|m| m.value.clone()).collect();

    assert_eq!(member_vals.len(), 3);
    assert!(matches!(member_vals[0], Value::Integer(1)));
    assert!(matches!(member_vals[1], Value::Integer(2)));
    assert!(matches!(member_vals[2], Value::Integer(3)));

    // Check enum members are in source map
    for member in members {
        oracle.source_map.get_value(member.id);
    }
}

#[test]
fn parse_empty_enum() {
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

    let enum_type = match oracle.types.get(id) {
        Type::Enum(e) => e,
        _ => panic!(),
    };
    oracle.source_map.get_type(id);

    // Check enum members array is empty
    let members = match oracle.values.get(enum_type.values) {
        Value::Array(arr) => arr,
        _ => panic!(),
    };

    assert_eq!(members.items().count(), 0);
}

#[test]
fn parse_invalid_enum() {
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
    let id = oracle.lower_type(&typ).unwrap();

    let enum_type = match oracle.types.get(id) {
        Type::Enum(e) => e,
        _ => panic!(),
    };
    oracle.source_map.get_type(id);

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

    let members: Vec<_> = match oracle.values.get(enum_type.values) {
        Value::Array(arr) => arr.items().collect(),
        _ => panic!(),
    };

    assert_eq!(members.len(), 1);
}

#[test]
fn parse_simple_union() {
    let text = indoc! {r#"
            type Foo: union ("type") {
                foo: Foo

                // Default should be ignored
                @default(69420)
                bar: Bar
            }
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
    let id = oracle.lower_type(&typ).unwrap();

    assert_eq!(oracle.reports(), vec![]);

    // Check union was inserted and is in source map
    let union = match oracle.types.get(id) {
        Type::Union(u) => u,
        _ => panic!(),
    };
    oracle.source_map.get_type(id);

    // Check discriminator
    let discriminator = union.disriminator.unwrap();
    assert_eq!(oracle.strings.get(discriminator), "type");

    let fields: Vec<_> = union.fields.collect();

    // Check field names are correct
    let names: Vec<_> = fields
        .iter()
        .map(|field| oracle.strings.get(field.name))
        .collect();
    assert_eq!(names, vec!["foo", "bar"]);

    // Check field @default's are correct
    assert!(fields.iter().all(|field| field.default.is_none()));

    // Check fields and field types are in source map
    for field in &fields {
        oracle.source_map.get_type(field.id.type_id());
    }
}

#[test]
fn parse_empty_union() {
    let text = indoc! {r#"
            type Foo: union ("kind") {}
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let typ = res.root.type_definitions().next().unwrap().typ().unwrap();
    let id = oracle.lower_type(&typ).unwrap();

    assert_eq!(oracle.reports(), vec![]);

    let union = match oracle.types.get(id) {
        Type::Union(u) => u,
        _ => panic!(),
    };
    oracle.source_map.get_type(id);

    // Check discriminator
    let discriminator = union.disriminator.unwrap();
    assert_eq!(oracle.strings.get(discriminator), "kind");

    assert_eq!(union.fields.count(), 0);
}

#[test]
fn parse_invalid_union() {
    let text = indoc! {r#"
            type Foo: union (42) {
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
                Report::error("union discriminator must be a string, got integer".to_string()).add_label(Label::primary("invalid union discriminator".to_string(), Span::new(17, 19))).with_note("help: union discrminator must be a string".to_string()),
                Report::error("inline record not allowed in union field".to_string()).add_label(
                    Label::primary("inline record type not allowed".to_string(), Span::new(36, 42))
                ).with_note("help: define a named type first, then reference it\n      example: `type MyRecord: rec { ... }`".to_string())
            ]
        );

    let union = match oracle.types.get(id) {
        Type::Union(u) => u,
        _ => panic!(),
    };
    oracle.source_map.get_type(id);

    // Check discriminator
    assert!(union.disriminator.is_none());

    assert_eq!(union.fields.count(), 0);
}
