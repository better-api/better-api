use better_api_syntax::{TextRange, TextSize};

use super::text_range_for_str;
use crate::path::PathParamIterator;

fn collect_params(part: &str) -> Vec<(&str, TextRange)> {
    let input_range = text_range_for_str(part);
    PathParamIterator::new(part, input_range).collect()
}

fn expected_param_range(start: u32, end: u32) -> TextRange {
    TextRange::new(TextSize::new(start), TextSize::new(end))
}

#[test]
fn empty_string_has_no_params() {
    let params = collect_params("");

    assert!(params.is_empty());
}

#[test]
fn unfinished_param_has_no_params() {
    let params = collect_params("foo/{bar");

    assert!(params.is_empty());
}

#[test]
fn single_param_returns_name_and_range() {
    let params = collect_params("{foo}");

    assert_eq!(params, vec![("foo", expected_param_range(2, 5))]);
}

#[test]
fn multiple_params_return_names_and_ranges() {
    let params = collect_params("/foo/{id}/bar/{slug}");

    assert_eq!(
        params,
        vec![
            ("id", expected_param_range(7, 9)),
            ("slug", expected_param_range(16, 20)),
        ]
    );
}

#[test]
fn iterator_skips_unfinished_param_after_valid_one() {
    let params = collect_params("/{id}/{unfinished/{finished}");

    assert_eq!(
        params,
        vec![
            ("id", expected_param_range(3, 5)),
            ("unfinished/{finished", expected_param_range(8, 28))
        ]
    );
}
