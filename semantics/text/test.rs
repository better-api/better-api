use better_api_diagnostic::{Label, Report, Span};
use better_api_syntax::ast::{self};
use better_api_syntax::{Parse, TextRange, TextSize, parse, tokenize};

use crate::text::{parse_string, validate_name};

#[test]
fn valid_name() {
    assert!(
        validate_name(
            "this-is-a.valid_name123",
            TextRange::new(TextSize::new(0), TextSize::new(1))
        )
        .is_ok(),
    );
}

#[test]
fn invalid_name() {
    let expected = Err(
                Report::error("invalid name".to_string())
                .add_label(Label::primary("invalid name".to_string(), Span::new(0, 1)))
                    .with_note("help: name can only contain alphanumeric characters, `_`, `-` and `.`. It also has to start with alphabetic character.".to_string()));

    assert_eq!(
        validate_name(
            "invalid name",
            TextRange::new(TextSize::new(0), TextSize::new(1))
        ),
        expected
    );
}

// Auxiliary function that gets first string token in tree.
fn get_string_token(res: &Parse) -> ast::StringToken {
    let val = res.root.api_names().next().unwrap().value().unwrap();
    match val {
        ast::Value::String(string) => string.string(),
        _ => panic!(),
    }
}

#[test]
fn valid_string() {
    let mut diags = vec![];
    let tokens = tokenize("name: \"foo\"", &mut diags);
    let res = parse(tokens);
    let token = get_string_token(&res);

    let res = parse_string(&token, Some(&mut diags));
    assert_eq!(diags, vec![]);
    assert_eq!(res, "foo");
}

#[test]
fn invalid_string() {
    let mut diags = vec![];
    let tokens = tokenize("name: \"inval\\id\"", &mut diags);
    let res = parse(tokens);
    let token = get_string_token(&res);

    let res = parse_string(&token, Some(&mut diags));
    assert_eq!(
        diags,
        vec![
            Report::error("got invalid escape character `i`".to_string()).add_label(
                Label::primary("invalid escape character".to_string(), Span::new(12, 14))
            )
        ]
    );
    assert_eq!(res, "invalid");
}

#[test]
fn valid_string_with_escapes() {
    let mut diags = vec![];
    let tokens = tokenize(r#"name: "valid \t \n \\ \" foo""#, &mut diags);
    let res = parse(tokens);
    let token = get_string_token(&res);

    let res = parse_string(&token, Some(&mut diags));
    assert_eq!(diags, vec![]);
    assert_eq!(res, "valid \t \n \\ \" foo");
}
