use better_api_syntax::{ast, parse, tokenize, Parse};
use indoc::indoc;

use crate::analyzer::Analyzer;

#[test]
fn compare_happy_path_primitives() {
    let text = indoc! {r#"
        type Defaults: rec {
            @default(-1)
            i32_field: i32

            @default(2)
            i64_field: i64

            @default(3)
            u32_field: u32

            @default(4)
            u64_field: u64

            @default(4.2)
            f32_field: f32

            @default(5.3)
            f64_field: f64

            @default(true)
            bool_field: bool

            @default("asdf")
            string_field: string

            @default("2026-03-23")
            date_field: date

            @default("2026-03-23T10:20:30Z")
            timestamp_field: timestamp
        }
    "#};

    let res = parse_text(text);
    let analyzer = setup_analyzer(&res.root);
    assert!(analyzer.reports.is_empty());
}

#[test]
fn compare_happy_path_composite_types() {
    let text = indoc! {r#"
        type InnerRecord: rec {
            foo: string
            bar: i32?
        }

        type Kind: enum (string) {
            "alpha"
            "beta"
        }

        type Payload: union {
            int_case: i32
            record_case: InnerRecord
        }

        type Defaults: rec {
            @default([1, 2, 3])
            arr_field: [i32]

            @default({
                foo: "hello"
                bar: 123
            })
            record_field: InnerRecord

            @default({
                "type": "int_case"
                data: 123
            })
            union_field: Payload

            @default("alpha")
            enum_field: Kind

            @default(null)
            option_null_field: i32?

            @default(123)
            option_value_field: i32?
        }
    "#};

    let res = parse_text(text);
    let analyzer = setup_analyzer(&res.root);
    assert!(analyzer.reports.is_empty());
}

#[test]
fn compare_happy_path_integer_enum() {
    let text = indoc! {r#"
        type Kind: enum (i32) {
            1
            2
            3
        }

        type Defaults: rec {
            @default(2)
            field: Kind
        }
    "#};

    let res = parse_text(text);
    let analyzer = setup_analyzer(&res.root);
    assert!(analyzer.reports.is_empty());
}

#[test]
fn compare_integer_enum_mismatch() {
    let text = indoc! {r#"
        type Kind: enum (i32) {
            1
            2
        }

        type Defaults: rec {
            @default(3)
            field: Kind
        }
    "#};

    let res = parse_text(text);
    let analyzer = setup_analyzer(&res.root);
    insta::assert_debug_snapshot!(analyzer.reports);
}

#[test]
fn compare_happy_path_reference_cycle() {
    let text = indoc! {r#"
        type Tree: rec {
            value: i32
            left: Tree?
            right: Tree?
        }

        type AliasA: Tree
        type AliasB: AliasA

        type Defaults: rec {
            @default({
                value: 1
                left: {
                    value: 2
                    left: null
                    right: null
                }
                right: null
            })
            root: AliasB
        }
    "#};

    let res = parse_text(text);
    let analyzer = setup_analyzer(&res.root);
    assert!(analyzer.reports.is_empty());
}

#[test]
fn compare_integer_ranges_are_validated() {
    let text = indoc! {r#"
        type Defaults: rec {
            @default(-2147483648)
            i32_min: i32

            @default(2147483647)
            i32_max: i32

            @default(-9223372036854775808)
            i64_min: i64

            @default(9223372036854775807)
            i64_max: i64

            @default(0)
            u32_min: u32

            @default(4294967295)
            u32_max: u32

            @default(0)
            u64_min: u64

            @default(18446744073709551615)
            u64_max: u64
        }
    "#};

    let res = parse_text(text);
    let analyzer = setup_analyzer(&res.root);
    assert!(analyzer.reports.is_empty());
}

#[test]
fn compare_integer_out_of_range() {
    let text = indoc! {r#"
        type Defaults: rec {
            @default(2147483648)
            i32_over: i32

            @default(-2147483649)
            i32_under: i32

            @default(9223372036854775808)
            i64_over: i64

            @default(-9223372036854775809)
            i64_under: i64

            @default(-1)
            u32_negative: u32

            @default(4294967296)
            u32_over: u32

            @default(-1)
            u64_negative: u64

            @default(18446744073709551616)
            u64_over: u64
        }
    "#};

    let res = parse_text(text);
    let analyzer = setup_analyzer(&res.root);
    insta::assert_debug_snapshot!(analyzer.reports);
}

#[test]
fn compare_primitive_mismatch() {
    let text = indoc! {r#"
        type Defaults: rec {
            @default(1)
            field: string
        }
    "#};

    let res = parse_text(text);
    let analyzer = setup_analyzer(&res.root);
    insta::assert_debug_snapshot!(analyzer.reports);
}

#[test]
fn compare_file_always_fails() {
    let text = indoc! {r#"
        type Defaults: rec {
            @default("foo.txt")
            string_value: file

            @default(null)
            null_value: file
        }
    "#};

    let res = parse_text(text);
    let analyzer = setup_analyzer(&res.root);
    insta::assert_debug_snapshot!(analyzer.reports);
}

#[test]
fn compare_null_expected_string() {
    let text = indoc! {r#"
        type Defaults: rec {
            @default(null)
            field: string
        }
    "#};

    let res = parse_text(text);
    let analyzer = setup_analyzer(&res.root);
    insta::assert_debug_snapshot!(analyzer.reports);
}

#[test]
fn compare_array_mismatch() {
    let text = indoc! {r#"
        type Defaults: rec {
            @default([1, "oops", 3])
            field: [i32]
        }
    "#};

    let res = parse_text(text);
    let analyzer = setup_analyzer(&res.root);
    insta::assert_debug_snapshot!(analyzer.reports);
}

#[test]
fn compare_option_inner_mismatch() {
    let text = indoc! {r#"
        type Defaults: rec {
            @default("oops")
            field: i32?
        }
    "#};

    let res = parse_text(text);
    let analyzer = setup_analyzer(&res.root);
    insta::assert_debug_snapshot!(analyzer.reports);
}

#[test]
fn compare_date_and_timestamp_mismatch() {
    let text = indoc! {r#"
        type Defaults: rec {
            @default("not-a-date")
            date_field: date

            @default("not-a-timestamp")
            timestamp_field: timestamp
        }
    "#};

    let res = parse_text(text);
    let analyzer = setup_analyzer(&res.root);
    insta::assert_debug_snapshot!(analyzer.reports);
}

#[test]
fn compare_enum_mismatch() {
    let text = indoc! {r#"
        type Kind: enum (string) {
            "alpha"
        }

        type Defaults: rec {
            @default("beta")
            field: Kind
        }
    "#};

    let res = parse_text(text);
    let analyzer = setup_analyzer(&res.root);
    insta::assert_debug_snapshot!(analyzer.reports);
}

#[test]
fn compare_object_record_field_mismatches() {
    let text = indoc! {r#"
        type Rec: rec {
            foo: string
        }

        type Defaults: rec {
            @default({
                foo: "ok"
                extra: 1
            })
            extra_field: Rec

            @default({})
            missing_field: Rec

            @default({
                foo: 1
            })
            invalid_field: Rec
        }
    "#};

    let res = parse_text(text);
    let analyzer = setup_analyzer(&res.root);
    insta::assert_debug_snapshot!(analyzer.reports);
}

#[test]
fn compare_union_missing_type_and_data() {
    let text = indoc! {r#"
        type Payload: union {
            foo: i32
        }

        type Defaults: rec {
            @default({
                data: 1
            })
            missing_type: Payload

            @default({
                "type": "foo"
            })
            missing_data: Payload
        }
    "#};

    let res = parse_text(text);
    let analyzer = setup_analyzer(&res.root);
    insta::assert_debug_snapshot!(analyzer.reports);
}

#[test]
fn compare_union_invalid_type() {
    let text = indoc! {r#"
        type Payload: union {
            foo: i32
        }

        type Defaults: rec {
            @default({
                "type": "bar"
                "data": 1
            })
            field: Payload
        }
    "#};

    let res = parse_text(text);
    let analyzer = setup_analyzer(&res.root);
    insta::assert_debug_snapshot!(analyzer.reports);
}

#[test]
fn compare_union_invalid_data() {
    let text = indoc! {r#"
        type Payload: union {
            foo: i32
        }

        type Defaults: rec {
            @default({
                "type": "foo"
                "data": "oops"
            })
            field: Payload
        }
    "#};

    let res = parse_text(text);
    let analyzer = setup_analyzer(&res.root);
    insta::assert_debug_snapshot!(analyzer.reports);
}

#[test]
fn compare_union_invalid_extra_field() {
    let text = indoc! {r#"
        type Payload: union {
            bar: i32
        }

        type Defaults: rec {
            @default({
                "type": "bar"
                "data": 1
                foo: 1
            })
            field: Payload
        }
    "#};

    let res = parse_text(text);
    let analyzer = setup_analyzer(&res.root);
    insta::assert_debug_snapshot!(analyzer.reports);
}

#[test]
fn compare_union_type_value_not_string() {
    let text = indoc! {r#"
        type Payload: union {
            foo: i32
        }

        type Defaults: rec {
            @default({
                "type": 1
                "data": 1
            })
            field: Payload
        }
    "#};

    let res = parse_text(text);
    let analyzer = setup_analyzer(&res.root);
    insta::assert_debug_snapshot!(analyzer.reports);
}

fn parse_text(text: &str) -> Parse {
    let mut diagnostics = vec![];
    let tokens: Vec<_> = tokenize(text, &mut diagnostics).collect();
    assert!(
        diagnostics.is_empty(),
        "unexpected tokenizer diagnostics: {:?}",
        &diagnostics
    );

    let res = parse(tokens.into_iter());
    assert!(
        res.reports.is_empty(),
        "unexpected parser reports: {:?}",
        &res.reports
    );
    res
}

fn setup_analyzer<'a>(root: &'a ast::Root) -> Analyzer<'a> {
    let mut analyzer = Analyzer::new(root);
    analyzer.validate_symbols();
    analyzer.lower_type_definitions();
    analyzer
}
