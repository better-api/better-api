use indoc::indoc;

use crate::{parse, tokenize};

#[test]
fn invalid_root_field() {
    let text = indoc! {r#"
        invalidField: foobar
        name: "hello"
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);

    let res = parse(tokens);
    insta::assert_debug_snapshot!(res.node);
    insta::assert_debug_snapshot!(res.reports);
}

#[test]
fn invalid_token() {
    let text = indoc! {r#"
        _invalid_token: foobar
        name: "hello"
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);

    let res = parse(tokens);
    insta::assert_debug_snapshot!(res.node);
    insta::assert_debug_snapshot!(res.reports);
}

#[test]
fn inline_comments_work() {
    let text = indoc! {r#"
        type Foo: rec {
            first: string // Inline comment before EOL
            second: i32 // Another inline comment
        }

        name: "hello" // Inline comment at root
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);

    let res = parse(tokens);
    insta::assert_debug_snapshot!(res.node);
    assert!(res.reports.is_empty());
}

#[test]
fn inline_doc_comments_work() {
    let text = indoc! {r#"
        type Foo: rec {
            @default("world") /// Inline doc comment attached to default
            greeting: string
        }
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);

    let res = parse(tokens);
    insta::assert_debug_snapshot!(res.node);
    insta::assert_debug_snapshot!(res.reports);
}

#[test]
fn invalid_doc_comments_emit_warnings() {
    let text = indoc! {r#"
        type Resp: resp {
            /// This doc comment should be ignored
            body: string
        }

        name: "hello" /// This inline doc comment should be ignored
    "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);

    let res = parse(tokens);
    insta::assert_debug_snapshot!(res.node);
    insta::assert_debug_snapshot!(res.reports);
}
