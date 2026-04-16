use better_api_diagnostic::{Label, Report, Span};
use better_api_syntax::{Parse, ast, parse, tokenize};

use indoc::indoc;

use crate::{
    analyzer::SpecLowerer,
    spec::{arena::value::ValueId, view::value::ValueView},
};

#[test]
fn lower_primitive_values() {
    let text = indoc! {r#"
        name: 10
        name: 4.20
        name: true
        name: false
        name: null
    "#};

    let res = parse_text(text);

    let mut analyzer = SpecLowerer::new(&res.root);
    let ids = lower_values(&mut analyzer, &res.root);
    let spec = analyzer.into_lowered_spec();

    let values: Vec<_> = ids.into_iter().map(|id| spec.spec.get_value(id)).collect();

    assert!(spec.reports.is_empty());
    assert!(matches!(values[0], ValueView::Integer(10)));
    assert!(matches!(values[1], ValueView::Float(4.2)));
    assert!(matches!(values[2], ValueView::Bool(true)));
    assert!(matches!(values[3], ValueView::Bool(false)));
    assert!(matches!(values[4], ValueView::Null));
}

#[test]
fn lower_string_values() {
    let text = indoc! {r#"
        name: "hello world"
        name: "hello\nworld\t\"\\"
    "#};

    let res = parse_text(text);

    let mut analyzer = SpecLowerer::new(&res.root);
    let ids = lower_values(&mut analyzer, &res.root);
    let spec = analyzer.into_lowered_spec();
    let values: Vec<_> = ids.into_iter().map(|id| spec.spec.get_value(id)).collect();

    assert!(spec.reports.is_empty());
    assert!(matches!(values[0], ValueView::String("hello world")));
    assert!(matches!(values[1], ValueView::String("hello\nworld\t\"\\")));
}

#[test]
fn lower_array_values() {
    let text = indoc! {r#"
        name: [1, 2, 3]
        name: []
        name: [1, [2, 3], 4]
    "#};

    let res = parse_text(text);

    let mut analyzer = SpecLowerer::new(&res.root);
    let ids = lower_values(&mut analyzer, &res.root);
    let spec = analyzer.into_lowered_spec();

    assert!(spec.reports.is_empty());

    let simple = match spec.spec.get_value(ids[0]) {
        ValueView::Array(array) => array,
        value => panic!("expected array, got {value:?}"),
    };
    let simple_items: Vec<_> = simple.items().map(|item| item.value).collect();
    assert_eq!(simple_items.len(), 3);
    assert!(matches!(simple_items[0], ValueView::Integer(1)));
    assert!(matches!(simple_items[1], ValueView::Integer(2)));
    assert!(matches!(simple_items[2], ValueView::Integer(3)));

    let empty = match spec.spec.get_value(ids[1]) {
        ValueView::Array(array) => array,
        value => panic!("expected array, got {value:?}"),
    };
    assert_eq!(empty.items().count(), 0);

    let nested = match spec.spec.get_value(ids[2]) {
        ValueView::Array(array) => array,
        value => panic!("expected array, got {value:?}"),
    };
    let nested_items: Vec<_> = nested.items().map(|item| item.value).collect();
    assert_eq!(nested_items.len(), 3);
    assert!(matches!(nested_items[0], ValueView::Integer(1)));
    assert!(matches!(nested_items[2], ValueView::Integer(4)));

    let inner = match nested_items[1].clone() {
        ValueView::Array(array) => array,
        value => panic!("expected nested array, got {value:?}"),
    };
    let inner_items: Vec<_> = inner.items().map(|item| item.value).collect();
    assert_eq!(inner_items.len(), 2);
    assert!(matches!(inner_items[0], ValueView::Integer(2)));
    assert!(matches!(inner_items[1], ValueView::Integer(3)));
}

#[test]
fn lower_empty_object() {
    let res = parse_text("name: {}");

    let mut analyzer = SpecLowerer::new(&res.root);
    let id = lower_first_value(&mut analyzer, &res.root);
    let spec = analyzer.into_lowered_spec();

    assert!(spec.reports.is_empty());

    let object = match spec.spec.get_value(id) {
        ValueView::Object(object) => object,
        value => panic!("expected object, got {value:?}"),
    };
    assert_eq!(object.fields().count(), 0);
}

#[test]
fn lower_object_fields_are_stable() {
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

    let res = parse_text(text);

    let mut analyzer = SpecLowerer::new(&res.root);
    let ids = lower_values(&mut analyzer, &res.root);
    let spec = analyzer.into_lowered_spec();

    for id in ids {
        let object = match spec.spec.get_value(id) {
            ValueView::Object(object) => object,
            value => panic!("expected object, got {value:?}"),
        };

        let fields: Vec<_> = object.fields().collect();
        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0].name.as_str(), "foo");
        assert_eq!(fields[1].name.as_str(), "bar");
        assert!(matches!(fields[0].value, ValueView::Integer(10)));
        assert!(matches!(fields[1].value, ValueView::String("test")));
    }

    assert!(spec.reports.is_empty());
}

#[test]
fn lower_object_with_string_keys() {
    let text = indoc! {r#"
        name: {
            "foo-bar": 10
            "baz": 20
        }
    "#};

    let res = parse_text(text);

    let mut analyzer = SpecLowerer::new(&res.root);
    let id = lower_first_value(&mut analyzer, &res.root);
    let spec = analyzer.into_lowered_spec();

    assert!(spec.reports.is_empty());

    let object = match spec.spec.get_value(id) {
        ValueView::Object(object) => object,
        value => panic!("expected object, got {value:?}"),
    };
    let fields: Vec<_> = object.fields().collect();

    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].name.as_str(), "foo-bar");
    assert_eq!(fields[1].name.as_str(), "baz");
    assert!(matches!(fields[0].value, ValueView::Integer(10)));
    assert!(matches!(fields[1].value, ValueView::Integer(20)));
}

#[test]
fn lower_object_skips_missing_value() {
    let text = indoc! {r#"
        name: {
            foo:
            bar: 20
        }
    "#};

    let res = parse_text(text);

    let mut analyzer = SpecLowerer::new(&res.root);
    let id = lower_first_value(&mut analyzer, &res.root);
    let spec = analyzer.into_lowered_spec();

    assert!(spec.reports.is_empty());

    let object = match spec.spec.get_value(id) {
        ValueView::Object(object) => object,
        value => panic!("expected object, got {value:?}"),
    };
    let fields: Vec<_> = object.fields().collect();

    assert_eq!(fields.len(), 1);
    assert_eq!(fields[0].name.as_str(), "bar");
    assert!(matches!(fields[0].value, ValueView::Integer(20)));
}

#[test]
fn lower_object_reports_invalid_field_name() {
    let text = indoc! {r#"
        name: {
            "invalid name": 10
            valid: 20
        }
    "#};

    let res = parse_text(text);

    let mut analyzer = SpecLowerer::new(&res.root);
    let id = lower_first_value(&mut analyzer, &res.root);
    let spec = analyzer.into_lowered_spec();

    assert_eq!(
        spec.reports,
        vec![
            Report::error("invalid name".to_string())
                .add_label(Label::primary("invalid name".to_string(), Span::new(12, 26)))
                .with_note(
                    "help: name can only contain alphanumeric characters, `_`, `-` and `.`. It also has to start with alphabetic character.".to_string()
                )
        ]
    );

    let object = match spec.spec.get_value(id) {
        ValueView::Object(object) => object,
        value => panic!("expected object, got {value:?}"),
    };
    let fields: Vec<_> = object.fields().collect();

    assert_eq!(fields.len(), 1);
    assert_eq!(fields[0].name.as_str(), "valid");
    assert!(matches!(fields[0].value, ValueView::Integer(20)));
}

#[test]
fn lower_object_reports_duplicate_keys() {
    let text = indoc! {r#"
        name: {
            foo: 10
            bar: 20
            foo: 10
        }
    "#};

    let res = parse_text(text);

    let mut analyzer = SpecLowerer::new(&res.root);
    let id = lower_first_value(&mut analyzer, &res.root);
    let spec = analyzer.into_lowered_spec();

    assert_eq!(
        spec.reports,
        vec![
            Report::error("repeated object key `foo`".to_string()).add_label(Label::primary(
                "repeated object key".to_string(),
                Span::new(36, 39)
            ))
        ]
    );

    let object = match spec.spec.get_value(id) {
        ValueView::Object(object) => object,
        value => panic!("expected object, got {value:?}"),
    };
    let fields: Vec<_> = object.fields().collect();

    assert_eq!(fields.len(), 3);
    assert_eq!(fields[0].name.as_str(), "foo");
    assert_eq!(fields[1].name.as_str(), "foo");
    assert_eq!(fields[2].name.as_str(), "bar");
    assert!(matches!(fields[0].value, ValueView::Integer(10)));
    assert!(matches!(fields[1].value, ValueView::Integer(10)));
    assert!(matches!(fields[2].value, ValueView::Integer(20)));
}

#[test]
fn lower_nested_object() {
    let text = indoc! {r#"
        name: {
            outer: {
                inner: 42
            }
        }
    "#};

    let res = parse_text(text);

    let mut analyzer = SpecLowerer::new(&res.root);
    let id = lower_first_value(&mut analyzer, &res.root);
    let spec = analyzer.into_lowered_spec();

    assert!(spec.reports.is_empty());

    let object = match spec.spec.get_value(id) {
        ValueView::Object(object) => object,
        value => panic!("expected object, got {value:?}"),
    };
    let fields: Vec<_> = object.fields().collect();

    assert_eq!(fields.len(), 1);
    assert_eq!(fields[0].name.as_str(), "outer");

    let inner_object = match fields[0].value.clone() {
        ValueView::Object(object) => object,
        value => panic!("expected nested object, got {value:?}"),
    };
    let inner_fields: Vec<_> = inner_object.fields().collect();

    assert_eq!(inner_fields.len(), 1);
    assert_eq!(inner_fields[0].name.as_str(), "inner");
    assert!(matches!(inner_fields[0].value, ValueView::Integer(42)));
}

#[test]
fn lower_complex_nested_structure() {
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

    let res = parse_text(text);

    let mut analyzer = SpecLowerer::new(&res.root);
    let id = lower_first_value(&mut analyzer, &res.root);
    let spec = analyzer.into_lowered_spec();

    assert!(spec.reports.is_empty());

    let object = match spec.spec.get_value(id) {
        ValueView::Object(object) => object,
        value => panic!("expected object, got {value:?}"),
    };
    let fields: Vec<_> = object.fields().collect();

    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].name.as_str(), "users");
    assert_eq!(fields[1].name.as_str(), "metadata");

    let users = match fields[0].value.clone() {
        ValueView::Array(array) => array,
        value => panic!("expected users array, got {value:?}"),
    };
    let users: Vec<_> = users.items().map(|item| item.value).collect();
    assert_eq!(users.len(), 2);

    let user1 = match users[0].clone() {
        ValueView::Object(object) => object,
        value => panic!("expected user object, got {value:?}"),
    };
    let user1_fields: Vec<_> = user1.fields().collect();
    assert_eq!(user1_fields.len(), 2);
    assert_eq!(user1_fields[0].name.as_str(), "name");
    assert_eq!(user1_fields[1].name.as_str(), "age");
    assert!(matches!(user1_fields[0].value, ValueView::String("alice")));
    assert!(matches!(user1_fields[1].value, ValueView::Integer(30)));

    let user2 = match users[1].clone() {
        ValueView::Object(object) => object,
        value => panic!("expected user object, got {value:?}"),
    };
    let user2_fields: Vec<_> = user2.fields().collect();
    assert_eq!(user2_fields.len(), 2);
    assert_eq!(user2_fields[0].name.as_str(), "name");
    assert_eq!(user2_fields[1].name.as_str(), "age");
    assert!(matches!(user2_fields[0].value, ValueView::String("bob")));
    assert!(matches!(user2_fields[1].value, ValueView::Integer(25)));

    let metadata = match fields[1].value.clone() {
        ValueView::Object(object) => object,
        value => panic!("expected metadata object, got {value:?}"),
    };
    let metadata_fields: Vec<_> = metadata.fields().collect();
    assert_eq!(metadata_fields.len(), 2);
    assert_eq!(metadata_fields[0].name.as_str(), "count");
    assert_eq!(metadata_fields[1].name.as_str(), "active");
    assert!(matches!(metadata_fields[0].value, ValueView::Integer(2)));
    assert!(matches!(metadata_fields[1].value, ValueView::Bool(true)));
}

fn parse_text(text: &str) -> Parse {
    let mut diagnostics = vec![];
    let tokens: Vec<_> = tokenize(text, &mut diagnostics).collect();
    assert!(
        diagnostics.is_empty(),
        "unexpected tokenizer diagnostics: {diagnostics:?}"
    );
    parse(tokens.into_iter())
}

fn lower_first_value<'a>(analyzer: &mut SpecLowerer<'a>, root: &'a ast::Root) -> ValueId {
    root.api_names()
        .next()
        .unwrap()
        .value()
        .map(|value| analyzer.lower_value(&value))
        .unwrap()
}

fn lower_values<'a>(analyzer: &mut SpecLowerer<'a>, root: &'a ast::Root) -> Vec<ValueId> {
    root.api_names()
        .map(|name| analyzer.lower_value(&name.value().unwrap()))
        .collect()
}
