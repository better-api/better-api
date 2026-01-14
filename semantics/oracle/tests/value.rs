use better_api_diagnostic::{Label, Report, Span};
use better_api_syntax::{parse, tokenize};
use indoc::indoc;

use crate::Oracle;
use crate::value::Value;

#[test]
fn parse_primitive_integer() {
    let text = indoc! {r#"
            name: 10
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let value = res.root.api_names().next().unwrap().value().unwrap();
    let id = oracle.lower_value(&value);

    assert_eq!(oracle.reports(), vec![]);
    assert!(matches!(oracle.values.get(id), Value::Integer(10)));
    oracle.source_map.get_value(id);
}

#[test]
fn parse_primitive_float() {
    let text = indoc! {r#"
            name: 4.20
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let value = res.root.api_names().next().unwrap().value().unwrap();
    let id = oracle.lower_value(&value);

    assert_eq!(oracle.reports(), vec![]);
    assert!(matches!(oracle.values.get(id), Value::Float(4.20)));
    oracle.source_map.get_value(id);
}

#[test]
fn parse_primitive_bool() {
    let text = indoc! {r#"
            name: true
            name: false
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let mut api_names = res.root.api_names();

    let value1 = api_names.next().unwrap().value().unwrap();
    let id1 = oracle.lower_value(&value1);
    assert!(matches!(oracle.values.get(id1), Value::Bool(true)));
    oracle.source_map.get_value(id1);

    let value2 = api_names.next().unwrap().value().unwrap();
    let id2 = oracle.lower_value(&value2);
    assert!(matches!(oracle.values.get(id2), Value::Bool(false)));
    oracle.source_map.get_value(id2);

    assert_eq!(oracle.reports(), vec![]);
}

#[test]
fn parse_primitive_string() {
    let text = indoc! {r#"
            name: "hello world"
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let value = res.root.api_names().next().unwrap().value().unwrap();
    let id = oracle.lower_value(&value);

    assert_eq!(oracle.reports(), vec![]);

    match oracle.values.get(id) {
        Value::String(str_id) => {
            let resolved = oracle.strings.get(str_id);
            assert_eq!(resolved, "hello world");
        }
        other => panic!("expected string value, got {other:?}"),
    }
    oracle.source_map.get_value(id);
}

#[test]
fn parse_primitive_string_with_escapes() {
    let text = indoc! {r#"
            name: "hello\nworld\t\"\\"
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let value = res.root.api_names().next().unwrap().value().unwrap();
    let id = oracle.lower_value(&value);

    assert_eq!(oracle.reports(), vec![]);
    match oracle.values.get(id) {
        Value::String(str_id) => {
            let resolved = oracle.strings.get(str_id);
            assert_eq!(resolved, "hello\nworld\t\"\\");
        }
        other => panic!("expected string value, got {other:?}"),
    }
    oracle.source_map.get_value(id);
}

#[test]
fn parse_array() {
    let text = indoc! {r#"
            name: [1, 2, 3]
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let value = res.root.api_names().next().unwrap().value().unwrap();
    let id = oracle.lower_value(&value);

    assert_eq!(oracle.reports(), vec![]);

    let array = match oracle.values.get(id) {
        Value::Array(arr) => arr,
        other => panic!("expected array value, got {other:?}"),
    };

    let item_ids: Vec<_> = array.map(|it| it.id).collect();
    let values: Vec<_> = item_ids.iter().map(|&id| oracle.values.get(id)).collect();
    assert_eq!(values.len(), 3);
    assert!(matches!(values[0], Value::Integer(1)));
    assert!(matches!(values[1], Value::Integer(2)));
    assert!(matches!(values[2], Value::Integer(3)));

    // Verify array and all items are in source map
    oracle.source_map.get_value(id);
    for id in item_ids {
        oracle.source_map.get_value(id);
    }
}

#[test]
fn parse_empty_array() {
    let text = indoc! {r#"
            name: []
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let value = res.root.api_names().next().unwrap().value().unwrap();
    let id = oracle.lower_value(&value);

    assert_eq!(oracle.reports(), vec![]);
    match oracle.values.get(id) {
        Value::Array(arr) => assert_eq!(arr.count(), 0),
        other => panic!("expected array value, got {other:?}"),
    }

    oracle.source_map.get_value(id);
}

#[test]
fn parse_nested_array() {
    let text = indoc! {r#"
            name: [1, [2, 3], 4]
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let value = res.root.api_names().next().unwrap().value().unwrap();
    let id = oracle.lower_value(&value);

    assert_eq!(oracle.reports(), vec![]);

    let outer = match oracle.values.get(id) {
        Value::Array(arr) => arr,
        other => panic!("expected array value, got {other:?}"),
    };
    oracle.source_map.get_value(id);

    let outer_items: Vec<_> = outer.map(|it| (it.id, it.value)).collect();
    assert_eq!(outer_items.len(), 3);
    assert!(matches!(outer_items[0].1, Value::Integer(1)));
    assert!(matches!(outer_items[2].1, Value::Integer(4)));

    // Verify all outer items are in source map
    for (item_id, _) in &outer_items {
        oracle.source_map.get_value(*item_id);
    }

    let inner = match &outer_items[1].1 {
        Value::Array(arr) => arr.clone(),
        other => panic!("expected array value, got {other:?}"),
    };

    let inner_items: Vec<_> = inner.map(|it| (it.id, it.value)).collect();
    assert_eq!(inner_items.len(), 2);
    assert!(matches!(inner_items[0].1, Value::Integer(2)));
    assert!(matches!(inner_items[1].1, Value::Integer(3)));

    // Verify all inner items are in source map
    for (item_id, _) in &inner_items {
        oracle.source_map.get_value(*item_id);
    }
}

#[test]
fn parse_object_empty() {
    let text = indoc! {r#"
            name: {}
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let value = res.root.api_names().next().unwrap().value().unwrap();
    let id = oracle.lower_value(&value);

    assert_eq!(oracle.reports(), vec![]);
    match oracle.values.get(id) {
        Value::Object(obj) => {
            assert_eq!(obj.count(), 0);
        }
        other => panic!("expected object value, got {other:?}"),
    }
    oracle.source_map.get_value(id);
}

#[test]
fn parse_object_simple() {
    // Test parsing of simple objects.
    // We have two objects that are the same, but with different ordering of fields.
    // This test also checks that ordering is the same, which means we have stable
    // ordering.
    let text = indoc! {r#"
            name: {
                foo: 10
                bar: "test"
            }

            name: {
                bar: "test"
                foo: 10
            }
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    assert_eq!(res.root.api_names().count(), 2);
    for name in res.root.api_names() {
        let id = oracle.lower_value(&name.value().unwrap());

        assert_eq!(oracle.reports(), vec![]);
        let obj = match oracle.values.get(id) {
            Value::Object(obj) => obj,
            other => panic!("expected object value, got {other:?}"),
        };
        oracle.source_map.get_value(id);

        let fields: Vec<_> = obj.collect();
        assert_eq!(fields.len(), 2);

        let names: Vec<_> = fields
            .iter()
            .map(|field| oracle.strings.get(field.name))
            .collect();
        assert_eq!(names, vec!["foo", "bar"]);

        // Verify all fields and their values are in source map
        for field in &fields {
            oracle.source_map.get_value(field.id.value_id());
        }

        let values: Vec<_> = fields.into_iter().map(|field| field.value).collect();
        assert!(matches!(values[0], Value::Integer(10)));
        assert!(matches!(values[1], Value::String(_)));
    }
}

#[test]
fn parse_object_with_string_keys() {
    let text = indoc! {r#"
            name: {
                "foo-bar": 10
                "baz": 20
            }
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let value = res.root.api_names().next().unwrap().value().unwrap();
    let id = oracle.lower_value(&value);

    assert_eq!(oracle.reports(), vec![]);
    let obj = match oracle.values.get(id) {
        Value::Object(obj) => obj,
        other => panic!("expected object value, got {other:?}"),
    };
    oracle.source_map.get_value(id);

    let fields: Vec<_> = obj.collect();
    assert_eq!(fields.len(), 2);

    let names: Vec<_> = fields
        .iter()
        .map(|field| oracle.strings.get(field.name))
        .collect();
    assert_eq!(names, vec!["foo-bar", "baz"]);

    // Verify all fields and their values are in source map
    for field in &fields {
        oracle.source_map.get_value(field.id.value_id());
    }

    let values: Vec<_> = fields.into_iter().map(|field| field.value).collect();
    assert!(matches!(values[0], Value::Integer(10)));
    assert!(matches!(values[1], Value::Integer(20)));
}

#[test]
fn parse_object_missing_value() {
    let text = indoc! {r#"
            name: {
                foo:
                bar: 20
            }
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let value = res.root.api_names().next().unwrap().value().unwrap();
    let id = oracle.lower_value(&value);
    assert_eq!(oracle.reports(), vec![]);

    // Field with missing value should be skipped
    let obj = match oracle.values.get(id) {
        Value::Object(obj) => obj,
        other => panic!("expected object value, got {other:?}"),
    };
    oracle.source_map.get_value(id);

    let fields: Vec<_> = obj.collect();
    assert_eq!(fields.len(), 1);

    let field_name = oracle.strings.get(fields[0].name);
    assert_eq!(field_name, "bar");
    assert!(matches!(fields[0].value, Value::Integer(20)));

    oracle.source_map.get_value(fields[0].id.value_id());
}

#[test]
fn parse_object_invalid_field_name() {
    let text = indoc! {r#"
            name: {
                "invalid name": 10
                valid: 20
            }
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let value = res.root.api_names().next().unwrap().value().unwrap();
    let id = oracle.lower_value(&value);

    // Should have error for invalid name
    assert_eq!(
            oracle.reports(),
            vec![
                Report::error("invalid name".to_string())
                    .add_label(Label::primary("invalid name".to_string(), Span::new(12, 26)))
                    .with_note(
                        "help: name can only contain alphanumeric characters, `_`, `-` and `.`. It also has to start with alphabetic character.".to_string()
                    )
            ]
        );

    // Only valid field should be present
    let obj = match oracle.values.get(id) {
        Value::Object(obj) => obj,
        other => panic!("expected object value, got {other:?}"),
    };
    oracle.source_map.get_value(id);

    let fields: Vec<_> = obj.collect();
    assert_eq!(fields.len(), 1);

    let field_name = oracle.strings.get(fields[0].name);
    assert_eq!(field_name, "valid");
    assert!(matches!(fields[0].value, Value::Integer(20)));

    oracle.source_map.get_value(fields[0].id.value_id());
}

#[test]
fn parse_object_duplicate_keys() {
    use better_api_diagnostic::{Label, Report, Span};

    let text = indoc! {r#"
            name: {
                foo: 10
                bar: 20
                foo: 10
            }
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let value = res.root.api_names().next().unwrap().value().unwrap();
    let id = oracle.lower_value(&value);

    // Should have error for duplicate key
    assert_eq!(
        oracle.reports(),
        vec![
            Report::error("repeated object key `foo`".to_string()).add_label(Label::primary(
                "repeated object key".to_string(),
                Span::new(36, 39)
            ))
        ]
    );

    // All fields should still be present in the arena
    let obj = match oracle.values.get(id) {
        Value::Object(obj) => obj,
        other => panic!("expected object value, got {other:?}"),
    };
    oracle.source_map.get_value(id);

    let fields: Vec<_> = obj.collect();
    assert_eq!(fields.len(), 3);

    let names: Vec<_> = fields
        .iter()
        .map(|field| oracle.strings.get(field.name))
        .collect();
    assert_eq!(names, vec!["foo", "foo", "bar"]);

    // Verify all fields and their values are in source map
    for field in &fields {
        oracle.source_map.get_value(field.id.value_id());
    }

    let values: Vec<_> = fields.into_iter().map(|field| field.value).collect();
    assert!(matches!(values[0], Value::Integer(10)));
    assert!(matches!(values[1], Value::Integer(10)));
    assert!(matches!(values[2], Value::Integer(20)));
}

#[test]
fn parse_object_nested() {
    let text = indoc! {r#"
            name: {
                outer: {
                    inner: 42
                }
            }
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let value = res.root.api_names().next().unwrap().value().unwrap();
    let id = oracle.lower_value(&value);

    assert_eq!(oracle.reports(), vec![]);

    let obj = match oracle.values.get(id) {
        Value::Object(obj) => obj,
        other => panic!("expected object value, got {other:?}"),
    };
    oracle.source_map.get_value(id);

    let fields: Vec<_> = obj.collect();
    assert_eq!(fields.len(), 1);

    let field_name = oracle.strings.get(fields[0].name);
    assert_eq!(field_name, "outer");

    oracle.source_map.get_value(fields[0].id.value_id());

    // Get nested object from field value
    let inner_obj = match &fields[0].value {
        Value::Object(obj) => obj.clone(),
        other => panic!("expected nested object, got {other:?}"),
    };

    let inner_fields: Vec<_> = inner_obj.collect();
    assert_eq!(inner_fields.len(), 1);

    let inner_field_name = oracle.strings.get(inner_fields[0].name);
    assert_eq!(inner_field_name, "inner");
    assert!(matches!(inner_fields[0].value, Value::Integer(42)));

    oracle.source_map.get_value(inner_fields[0].id.value_id());
}

#[test]
fn parse_complex_nested_structure() {
    let text = indoc! {r#"
            name: {
                users: [
                    {
                        name: "alice"
                        age: 30
                    }
                    {
                        name: "bob"
                        age: 25
                    }
                ]
                metadata: {
                    count: 2
                    active: true
                }
            }
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let mut oracle = Oracle::new_raw(&res.root);

    let value = res.root.api_names().next().unwrap().value().unwrap();
    let id = oracle.lower_value(&value);
    assert_eq!(oracle.reports(), vec![]);

    let obj = match oracle.values.get(id) {
        Value::Object(obj) => obj,
        other => panic!("expected object value, got {other:?}"),
    };
    // Test element is in map. If it isn't get_bck panics.
    oracle.source_map.get_value(id);

    let fields: Vec<_> = obj.collect();
    assert_eq!(fields.len(), 2);

    // Check field names (ordering: users, metadata)
    let names: Vec<_> = fields
        .iter()
        .map(|field| oracle.strings.get(field.name))
        .collect();
    assert_eq!(names, vec!["users", "metadata"]);

    // Verify top-level fields are in source map
    for field in &fields {
        oracle.source_map.get_value(field.id.value_id());
    }

    // Check users array
    let users_arr = match &fields[0].value {
        Value::Array(arr) => arr.clone(),
        other => panic!("expected users array, got {other:?}"),
    };

    let users: Vec<_> = users_arr.map(|it| (it.id, it.value)).collect();
    assert_eq!(users.len(), 2);

    // Verify array elements are in source map
    for (user_id, _) in &users {
        oracle.source_map.get_value(*user_id);
    }

    // First user
    let user1_obj = match &users[0].1 {
        Value::Object(obj) => obj.clone(),
        other => panic!("expected user object, got {other:?}"),
    };
    let user1_fields: Vec<_> = user1_obj.collect();
    assert_eq!(user1_fields.len(), 2);

    let user1_names: Vec<_> = user1_fields
        .iter()
        .map(|field| oracle.strings.get(field.name))
        .collect();
    assert_eq!(user1_names, vec!["name", "age"]);

    // Verify user1 fields are in source map
    for field in &user1_fields {
        oracle.source_map.get_value(field.id.value_id());
    }

    match user1_fields[0].value {
        Value::String(str_id) => {
            assert_eq!(oracle.strings.get(str_id), "alice");
        }
        ref other => panic!("expected string, got {other:?}"),
    }
    assert!(matches!(user1_fields[1].value, Value::Integer(30)));

    // Second user
    let user2_obj = match &users[1].1 {
        Value::Object(obj) => obj.clone(),
        other => panic!("expected user object, got {other:?}"),
    };
    let user2_fields: Vec<_> = user2_obj.collect();
    assert_eq!(user2_fields.len(), 2);

    let user2_names: Vec<_> = user2_fields
        .iter()
        .map(|field| oracle.strings.get(field.name))
        .collect();
    assert_eq!(user2_names, vec!["name", "age"]);

    // Verify user2 fields are in source map
    for field in &user2_fields {
        oracle.source_map.get_value(field.id.value_id());
    }

    match user2_fields[0].value {
        Value::String(str_id) => {
            assert_eq!(oracle.strings.get(str_id), "bob");
        }
        ref other => panic!("expected string, got {other:?}"),
    }
    assert!(matches!(user2_fields[1].value, Value::Integer(25)));

    // Check metadata object
    let metadata_obj = match &fields[1].value {
        Value::Object(obj) => obj.clone(),
        other => panic!("expected metadata object, got {other:?}"),
    };

    let metadata_fields: Vec<_> = metadata_obj.collect();
    assert_eq!(metadata_fields.len(), 2);

    let metadata_names: Vec<_> = metadata_fields
        .iter()
        .map(|field| oracle.strings.get(field.name))
        .collect();
    assert_eq!(metadata_names, vec!["count", "active"]);

    // Verify metadata fields are in source map
    for field in &metadata_fields {
        oracle.source_map.get_value(field.id.value_id());
    }

    assert!(matches!(metadata_fields[0].value, Value::Integer(2)));
    assert!(matches!(metadata_fields[1].value, Value::Bool(true)));
}
