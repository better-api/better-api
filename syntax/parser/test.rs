use better_api_diagnostic::{Label, Report, Span};
use indoc::indoc;

use super::parse;
use crate::tokenize;

#[test]
fn parse_basic_info() {
    let text = indoc! {r#"
            //! This is a top comment
            // This is a normal comment
            //! Here is another top comment
            
            /// A doc comment
            name:  "foobar"

            // This should report an error
            @default("asdf")
            betterApi: "1.0.0"

            version: "1.0"
        "#};
    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);

    let (tree, diagnostics) = parse(tokens);
    insta::assert_debug_snapshot!(tree);
    assert_eq!(
        diagnostics,
        vec![
            Report::error("unexpected `@default`".to_string()).with_label(Label::new(
                "unexpected `@default`".to_string(),
                Span::new(153, 170)
            ))
        ]
    );
}

#[test]
fn parse_server() {
    let text = indoc! {r#"
            /// doc comment
            server: {
                name: "foo"  
                url: "bar"
            }
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);

    let (tree, diagnostics) = parse(tokens);
    insta::assert_debug_snapshot!(tree);
    assert_eq!(diagnostics, vec![]);
}

#[test]
fn parse_server_error() {
    let text = indoc! {r#"
            /// doc comment
            server: {
                name: foobar
                123: "bar"
                url: "baz"
            }
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);

    let (tree, diagnostics) = parse(tokens);
    insta::assert_debug_snapshot!(tree);
    insta::assert_debug_snapshot!(diagnostics);
}

#[test]
fn parse_value() {
    let text = indoc! {r#"
            name: false
            name: true
            name: "string"
            name: 69
            name: 4.20
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);

    let (tree, diagnostics) = parse(tokens);
    insta::assert_debug_snapshot!(tree);
    assert_eq!(diagnostics, vec![]);
}

#[test]
fn parse_simple_type_def() {
    let text = indoc! {r#"
            /// Doc comment 
            type Foo: Bar
            
            type Str: string
            type I32: i32
            type I64: i64
            type U32: u32
            type U64: u64
            type F32: f32
            type F64: f64
            type Date: date
            type TimeStamp: timestamp
            type Boolean: bool
            type File: file
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);

    let (tree, diagnostics) = parse(tokens);
    insta::assert_debug_snapshot!(tree);
    assert_eq!(diagnostics, vec![]);
}

#[test]
fn parse_option_type() {
    let text = indoc! {r#"
            type Foo: i32?
            type Foo: string   ?
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);

    let (tree, diagnostics) = parse(tokens);
    insta::assert_debug_snapshot!(tree);
    assert_eq!(diagnostics, vec![]);
}

#[test]
fn parse_array_type() {
    let text = indoc! {r#"
            type Foo: [i32]
            type Foo: [ string ?]
            type Foo: [string?]?
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);

    let (tree, diagnostics) = parse(tokens);
    insta::assert_debug_snapshot!(tree);
    assert_eq!(diagnostics, vec![]);
}

#[test]
fn parse_record_type() {
    let text = indoc! {r#"
            type Foo: rec {
                // comment
                /// doc comment
                foo: bool

                /// More doc comment
                @default("foobar")
                bar: string

                // Just a comment
                hey: i32

                hoy: rec {
                    @default(true)
                    nested: bool
                }
            }
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);

    let (tree, diagnostics) = parse(tokens);
    insta::assert_debug_snapshot!(tree);
    assert_eq!(diagnostics, vec![]);
}
