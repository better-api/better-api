use better_api_diagnostic::Severity;
use better_api_syntax::{parse, tokenize};

use super::text_range_for_str;
use crate::{
    path::{PathPart, validate_path},
    text::parse_string,
};

#[test]
fn valid_paths() {
    let paths = ["/foo", "/foo/bar", "/foo/{param_id}", "/foo/{p1}/{p2}"];

    for path in &paths {
        let mut diagnostics = vec![];
        let range = text_range_for_str(path);
        validate_path(path, range, &mut diagnostics);

        assert!(diagnostics.is_empty());
    }
}

#[test]
fn path_part_ordering_basic() {
    let left = PathPart::Segment("/a");
    let right = PathPart::Segment("/b");
    assert!(left < right);
    assert!(right > left);
    assert_eq!(PathPart::Segment("/a"), PathPart::Segment("/a"));
    assert_eq!(PathPart::Empty, PathPart::Empty);
    assert_eq!(PathPart::Segment("a"), PathPart::Segment("a"));
}

#[test]
fn path_part_new_basic() {
    let mut diagnostics = vec![];
    let range = text_range_for_str("/foo");
    let part = PathPart::new("/foo", range, &mut diagnostics);
    assert!(matches!(part, PathPart::Segment("/foo")));
    assert!(diagnostics.is_empty());

    let mut diagnostics = vec![];
    let range = text_range_for_str("");
    let part = PathPart::new("", range, &mut diagnostics);
    assert!(matches!(part, PathPart::Empty));
    insta::assert_debug_snapshot!(diagnostics);

    let mut diagnostics = vec![];
    let range = text_range_for_str("/");
    let part = PathPart::new("/", range, &mut diagnostics);
    assert!(matches!(part, PathPart::Empty));
    insta::assert_debug_snapshot!(diagnostics);
}

#[test]
fn path_part_new_trailing_slash() {
    // Test that trailing slash is stripped
    let mut diagnostics = vec![];
    let range = text_range_for_str("/foo/");
    let part = PathPart::new("/foo/", range, &mut diagnostics);
    assert!(matches!(part, PathPart::Segment("/foo")));
    // validate_path generates a warning about trailing slash
    assert!(
        diagnostics
            .first()
            .is_some_and(|diag| diag.severity == Severity::Warning)
    );

    let mut diagnostics = vec![];
    let range = text_range_for_str("/foo/bar/");
    let part = PathPart::new("/foo/bar/", range, &mut diagnostics);
    assert!(matches!(part, PathPart::Segment("/foo/bar")));
    // validate_path generates a warning about trailing slash
    assert!(
        diagnostics
            .first()
            .is_some_and(|diag| diag.severity == Severity::Warning)
    );

    let mut diagnostics = vec![];
    let range = text_range_for_str("/foo/{id}/");
    let part = PathPart::new("/foo/{id}/", range, &mut diagnostics);
    assert!(matches!(part, PathPart::Segment("/foo/{id}")));
    // validate_path generates a warning about trailing slash
    assert!(
        diagnostics
            .first()
            .is_some_and(|diag| diag.severity == Severity::Warning)
    );

    let mut diagnostics = vec![];
    let range = text_range_for_str("/{param}/");
    let part = PathPart::new("/{param}/", range, &mut diagnostics);
    assert!(matches!(part, PathPart::Segment("/{param}")));
    // validate_path generates a warning about trailing slash
    assert!(
        diagnostics
            .first()
            .is_some_and(|diag| diag.severity == Severity::Warning)
    );
}

#[test]
fn path_part_ordering_params_equal() {
    assert_eq!(PathPart::Segment("/{id}"), PathPart::Segment("/{name}"));
    assert_eq!(
        PathPart::Segment("/foo/{id}"),
        PathPart::Segment("/foo/{name}")
    );
}

#[test]
fn path_part_ordering_param_vs_literal() {
    assert!(PathPart::Segment("/{id}") > PathPart::Segment("/a"));
    assert!(PathPart::Segment("/foo/{id}") > PathPart::Segment("/foo/bar"));
}

#[test]
fn path_part_ordering_param_offset() {
    assert!(PathPart::Segment("/{foo}") > PathPart::Segment("/a{foo}"));
    assert_eq!(PathPart::Segment("/a{foo}"), PathPart::Segment("/a{bar}"));
}

#[test]
fn empty_segments() {
    let path = "/foo//bar";
    let mut diagnostics = vec![];
    let range = text_range_for_str(path);
    validate_path(path, range, &mut diagnostics);

    insta::assert_debug_snapshot!(diagnostics);
}

#[test]
fn empty_parameter() {
    let path = "/foo/{}";
    let mut diagnostics = vec![];
    let range = text_range_for_str(path);
    validate_path(path, range, &mut diagnostics);

    insta::assert_debug_snapshot!(diagnostics);
}

#[test]
fn invalid_parameter_position_start() {
    let path = "/foo{param}";
    let mut diagnostics = vec![];
    let range = text_range_for_str(path);
    validate_path(path, range, &mut diagnostics);

    insta::assert_debug_snapshot!(diagnostics);
}

#[test]
fn invalid_parameter_position_end() {
    let path = "/{param}bar";
    let mut diagnostics = vec![];
    let range = text_range_for_str(path);
    validate_path(path, range, &mut diagnostics);

    insta::assert_debug_snapshot!(diagnostics);
}

#[test]
fn invalid_trailing_slash() {
    let path = "/foo/bar/";
    let mut diagnostics = vec![];
    let range = text_range_for_str(path);
    validate_path(path, range, &mut diagnostics);

    insta::assert_debug_snapshot!(diagnostics);
}

#[test]
fn missing_leading_slash() {
    let path = "foo/bar";
    let mut diagnostics = vec![];
    let range = text_range_for_str(path);
    validate_path(path, range, &mut diagnostics);

    insta::assert_debug_snapshot!(diagnostics);
}

#[test]
fn invalid_chars_in_path() {
    let path = "/foo/bar baz";
    let mut diagnostics = vec![];
    let range = text_range_for_str(path);
    validate_path(path, range, &mut diagnostics);

    insta::assert_debug_snapshot!(diagnostics);
}

#[test]
fn invalid_chars_unicode() {
    let path = "/foo/café";
    let mut diagnostics = vec![];
    let range = text_range_for_str(path);
    validate_path(path, range, &mut diagnostics);

    insta::assert_debug_snapshot!(diagnostics);
}

#[test]
fn valid_chars_in_path() {
    let paths = [
        "/foo-bar",
        "/foo.bar",
        "/foo_bar",
        "/foo~bar",
        "/foo:bar",
        "/foo@bar",
        "/foo!bar",
        "/foo$bar",
        "/foo&bar",
        "/foo'bar",
        "/foo(bar)",
        "/foo*bar",
        "/foo+bar",
        "/foo,bar",
        "/foo;bar",
        "/foo=bar",
    ];

    for path in &paths {
        let mut diagnostics = vec![];
        let range = text_range_for_str(path);
        validate_path(path, range, &mut diagnostics);

        assert!(diagnostics.is_empty(), "Path {path} should be valid");
    }
}

#[test]
fn invalid_param_name_special_chars() {
    let path = "/foo/{param-name}";
    let mut diagnostics = vec![];
    let range = text_range_for_str(path);
    validate_path(path, range, &mut diagnostics);

    insta::assert_debug_snapshot!(diagnostics);
}

#[test]
fn invalid_param_name_starts_with_number() {
    let path = "/foo/{1param}";
    let mut diagnostics = vec![];
    let range = text_range_for_str(path);
    validate_path(path, range, &mut diagnostics);

    insta::assert_debug_snapshot!(diagnostics);
}

#[test]
fn valid_param_names() {
    let paths = [
        "/foo/{param}",
        "/foo/{paramName}",
        "/foo/{param123}",
        "/foo/{p}",
    ];

    for path in &paths {
        let mut diagnostics = vec![];
        let range = text_range_for_str(path);
        validate_path(path, range, &mut diagnostics);

        assert!(diagnostics.is_empty(), "Path {path} should be valid");
    }
}

#[test]
fn invalid_url_escape_incomplete() {
    let path = "/foo/%2";
    let mut diagnostics = vec![];
    let range = text_range_for_str(path);
    validate_path(path, range, &mut diagnostics);

    insta::assert_debug_snapshot!(diagnostics);
}

#[test]
fn invalid_url_escape_non_hex() {
    let path = "/foo/%ZZ";
    let mut diagnostics = vec![];
    let range = text_range_for_str(path);
    validate_path(path, range, &mut diagnostics);

    insta::assert_debug_snapshot!(diagnostics);
}

#[test]
fn invalid_url_escape_missing() {
    let path = "/foo/%";
    let mut diagnostics = vec![];
    let range = text_range_for_str(path);
    validate_path(path, range, &mut diagnostics);

    insta::assert_debug_snapshot!(diagnostics);
}

#[test]
fn valid_url_escapes() {
    let paths = ["/foo/%20bar", "/foo/%2F", "/foo/%aB", "/foo/%FF"];

    for path in &paths {
        let mut diagnostics = vec![];
        let range = text_range_for_str(path);
        validate_path(path, range, &mut diagnostics);

        assert!(diagnostics.is_empty(), "Path {path} should be valid");
    }
}

#[test]
fn multiple_errors() {
    let path = "foo//bar/{}/baz qux";
    let mut diagnostics = vec![];
    let range = text_range_for_str(path);
    validate_path(path, range, &mut diagnostics);

    insta::assert_debug_snapshot!(diagnostics);
}

// This tests that our assumption in what text_range that validate_path gets from the parse is.
// In other words, it checks that text_range_for_str is "mocking" ranges correctly.
//
// We do that by getting a path from the real world pipeline of
// tokenize -> parse -> get endpoint -> get path -> parse string token -> validate_path
#[test]
fn range_interpretation() {
    let text = indoc::indoc! {r#"
            GET "foo/bar/{in-valid}/" {}
        "#};

    let mut diagnostics = vec![];
    let tokens = tokenize(text, &mut diagnostics);
    let res = parse(tokens);

    let path = res.root.endpoints().next().unwrap().path().unwrap();
    let token = path.string();
    let path_str = parse_string(&token, &mut diagnostics);
    validate_path(&path_str, token.text_range(), &mut diagnostics);

    insta::assert_debug_snapshot!(diagnostics);
}

// Helper function to compute hash of a PathPart
fn hash_pathpart(part: &PathPart) -> u64 {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    let mut hasher = DefaultHasher::new();
    part.hash(&mut hasher);
    hasher.finish()
}

#[test]
fn hash_empty_pathpart() {
    let empty1 = PathPart::Empty;
    let empty2 = PathPart::Empty;
    assert_eq!(hash_pathpart(&empty1), hash_pathpart(&empty2));
}

#[test]
fn hash_basic_segments() {
    let part1 = PathPart::Segment("/foo");
    let part2 = PathPart::Segment("/foo");
    let part3 = PathPart::Segment("/bar");

    assert_eq!(hash_pathpart(&part1), hash_pathpart(&part2));
    assert_ne!(hash_pathpart(&part1), hash_pathpart(&part3));
}

#[test]
fn hash_params_with_different_names() {
    // Parameters with different names should hash to the same value
    let part1 = PathPart::Segment("/{id}");
    let part2 = PathPart::Segment("/{name}");
    let part3 = PathPart::Segment("/{user_id}");

    assert_eq!(hash_pathpart(&part1), hash_pathpart(&part2));
    assert_eq!(hash_pathpart(&part1), hash_pathpart(&part3));
}

#[test]
fn hash_params_in_path_segments() {
    // Paths with params in different positions should have different hashes
    let part1 = PathPart::Segment("/foo/{id}");
    let part2 = PathPart::Segment("/foo/{name}");
    let part3 = PathPart::Segment("/bar/{id}");

    assert_eq!(hash_pathpart(&part1), hash_pathpart(&part2));
    assert_ne!(hash_pathpart(&part1), hash_pathpart(&part3));
}

#[test]
fn hash_multiple_params() {
    let part1 = PathPart::Segment("/foo/{id}/bar/{name}");
    let part2 = PathPart::Segment("/foo/{user}/bar/{item}");
    let part3 = PathPart::Segment("/foo/{id}/baz/{name}");

    assert_eq!(hash_pathpart(&part1), hash_pathpart(&part2));
    assert_ne!(hash_pathpart(&part1), hash_pathpart(&part3));
}

#[test]
fn hash_empty_vs_segment() {
    // Empty and Segment should have different hashes
    let empty = PathPart::Empty;
    let segment = PathPart::Segment("/foo");

    assert_ne!(hash_pathpart(&empty), hash_pathpart(&segment));
}

#[test]
fn hash_partial_param_match() {
    // Partial params should hash differently based on position
    let part1 = PathPart::Segment("/a{foo}");
    let part2 = PathPart::Segment("/a{bar}");
    let part3 = PathPart::Segment("/b{foo}");

    assert_eq!(hash_pathpart(&part1), hash_pathpart(&part2));
    assert_ne!(hash_pathpart(&part1), hash_pathpart(&part3));
}

#[test]
fn hash_consistency_with_eq() {
    // If two PathParts are equal, they must have the same hash
    let test_cases = vec![
        (PathPart::Empty, PathPart::Empty),
        (PathPart::Segment("/foo"), PathPart::Segment("/foo")),
        (PathPart::Segment("/{id}"), PathPart::Segment("/{name}")),
        (
            PathPart::Segment("/foo/{id}"),
            PathPart::Segment("/foo/{user_id}"),
        ),
        (
            PathPart::Segment("/foo/{a}/bar/{b}"),
            PathPart::Segment("/foo/{x}/bar/{y}"),
        ),
        (PathPart::Segment("/a{foo}"), PathPart::Segment("/a{bar}")),
    ];

    for (left, right) in test_cases {
        if hash_pathpart(&left) == hash_pathpart(&right) {
            assert_eq!(left, right)
        }
    }
}

#[test]
fn hash_empty_param_edge_case() {
    // Edge case: what if someone has {} (which is invalid but let's test hash consistency)
    let part1 = PathPart::Segment("/foo/{}");
    let part2 = PathPart::Segment("/foo/{}");

    assert_eq!(hash_pathpart(&part1), hash_pathpart(&part2));
}

#[test]
fn hash_special_chars() {
    let part1 = PathPart::Segment("/foo-bar");
    let part2 = PathPart::Segment("/foo-bar");
    let part3 = PathPart::Segment("/foo_bar");

    assert_eq!(hash_pathpart(&part1), hash_pathpart(&part2));
    assert_ne!(hash_pathpart(&part1), hash_pathpart(&part3));
}
