//! Module for working with route and endpoint paths.
//!
//!
//! The core types in this module are [`PathPart`], [`Path`] and [`PathArena`].
//! [`Path`] is made of multiple [`PathPart`s](PathPart). The only way to construct a path
//! is through [`PathArena`]. After inserting one or more [`PathPart`s](PathPart) into
//! the [`PathArena`] you can get the final [`Path`] from it.
//!
//!
//! Path is made from current part and optional prefix. What exactly it represents is best
//! shown on an example. Let's say we have a nested endpoint:
//! ```text
//! route "/foo" {
//!     GET "/bar" {
//!         ...
//!     }
//! }
//! ```
//!
//! The full path of the inner endpoint is `/foo/bar`. This is represented with a following Path-like object:
//! ```text
//! Path {
//!     part: "/bar",
//!     prefix: Path {
//!         part: "/foo",
//!         prefix: None
//!     }
//! }
//! ```
//!
//! ## Usage
//!
//! The general usage is as follows:
//! 1. Construct a [`&PathPart`][PathPart] from `&str` using [`PathPart::new`]. This checks that
//!    the given string is a valid path part and reports possible errors.
//! 2. Store the part into [`PathArena`] with [`PathArena::insert`]. When storing a part you specify an optional prefix, which
//!    is a [`PathId`] of a previously stored path.
//! 3. Get the [`Path`] with [`PathArena::get`].
//!
//! ## Equality
//!
//! Paths can contain path parameters, which need to be taken into account when dealing with
//! equality. For instance `/foo/{bar}` and `/foo/{id}` are equal paths, even though the path
//! parameters are named differently. To the router and the caller the name of the parameters
//! doesn't matter, which is why these two paths are equal.
//!
//! The implementation of [`PartialEq`] and [`Hash`] takes this fact into account.

use std::hash::{Hash, Hasher};
use std::iter::Peekable;
use std::{cmp::Ordering, str::Chars};

use smallvec::SmallVec;

use better_api_diagnostic::{Label, Report, Span};
use better_api_syntax::TextRange;

/// A single part of the [`Path`].
///
/// See [module documentation](self) for more details.
#[derive(Debug, Clone, Copy)]
pub enum PathPart<'a> {
    Empty,
    Segment(&'a str),
}

/// Represents path inside a URL.
///
/// See [module documentation](self) for more details.
pub struct Path<'a> {
    arena: &'a PathArena,

    id: PathId,

    /// Tail part of the path
    part: PathPart<'a>,

    /// Id of the prefix
    prefix_id: Option<PathId>,
}

impl<'a> Path<'a> {
    /// Get prefix of the path
    pub fn prefix(&self) -> Option<Path<'a>> {
        self.prefix_id.map(|id| self.arena.get(id))
    }

    /// Get id of the path prefix
    pub fn prefix_id(&self) -> Option<PathId> {
        self.prefix_id
    }

    /// Get the tail part of the path
    pub fn part(&'a self) -> PathPart<'a> {
        self.part
    }

    /// Get id of the path
    pub fn id(&self) -> PathId {
        self.id
    }

    /// Get array of segments in the path
    pub fn segments(&self) -> SmallVec<[&'a str; 3]> {
        let mut res = SmallVec::new();
        self.collect_segments(&mut res);

        res
    }

    /// Helper function for collecting segments into small vector
    fn collect_segments(&self, segments: &mut SmallVec<[&'a str; 3]>) {
        if let Some(prefix) = self.prefix() {
            prefix.collect_segments(segments);
        }

        if let PathPart::Segment(seg) = self.part {
            segments.push(seg);
        }
    }
}

impl<'a> PathPart<'a> {
    /// Parses string into path part.
    ///
    /// This function checks that string is valid and reports the possible invalid characters
    /// or path parameters.
    pub fn new(string: &'a str, text_range: TextRange, diagnostics: &mut Vec<Report>) -> Self {
        if string.is_empty() || string == "/" {
            diagnostics.push(
                Report::warning("unecessary empty path specified".to_string())
                    .add_label(Label::primary(
                        "unecessary empty path specified".to_string(),
                        text_range.into(),
                    ))
                    .with_note(
                        "help: path should be omited instead of specifying an empty path"
                            .to_string(),
                    ),
            );
            return Self::Empty;
        }

        validate_path(string, text_range, diagnostics);
        Self::Segment(string)
    }
}

impl Ord for PathPart<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        let (mut left, mut right) = match (self, other) {
            (PathPart::Empty, PathPart::Empty) => return Ordering::Equal,
            (PathPart::Empty, PathPart::Segment(_)) => return Ordering::Less,
            (PathPart::Segment(_), PathPart::Empty) => return Ordering::Greater,
            (PathPart::Segment(l), PathPart::Segment(r)) => {
                (l.chars().peekable(), r.chars().peekable())
            }
        };

        loop {
            match (left.next(), right.next()) {
                (None, None) => return Ordering::Equal,
                (Some(l), None) => {
                    // Trailing slash is unnecessary and ignored
                    return if l == '/' && left.peek().is_none() {
                        Ordering::Equal
                    } else {
                        Ordering::Greater
                    };
                }
                (None, Some(r)) => {
                    // Trailing slash is unnecessary and ignored
                    return if r == '/' && right.peek().is_none() {
                        Ordering::Equal
                    } else {
                        Ordering::Less
                    };
                }
                (Some(l), Some(r)) => {
                    let is_left_param = if l == '{' {
                        skip_param(&mut left);
                        true
                    } else {
                        false
                    };

                    let is_right_param = if r == '{' {
                        skip_param(&mut right);
                        true
                    } else {
                        false
                    };

                    match (is_left_param, is_right_param) {
                        (true, true) => continue,
                        (true, false) => return Ordering::Greater,
                        (false, true) => return Ordering::Less,
                        (false, false) => match l.cmp(&r) {
                            Ordering::Equal => continue,
                            ord => return ord,
                        },
                    }
                }
            }
        }
    }
}

fn skip_param(chars: &mut Peekable<Chars>) {
    for c in chars {
        if c == '}' {
            break;
        }
    }
}

impl PartialOrd for PathPart<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for PathPart<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for PathPart<'_> {}

impl Hash for PathPart<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            PathPart::Empty => {
                // Hash a discriminant for Empty variant
                0u8.hash(state);
            }
            PathPart::Segment(s) => {
                // Hash a discriminant for Segment variant
                1u8.hash(state);

                // Iterate through chars and hash them, treating params specially
                let mut chars = s.chars().peekable();
                while let Some(ch) = chars.next() {
                    if ch == '{' {
                        // Hash the placeholder for any parameter
                        '{'.hash(state);
                        '}'.hash(state);
                        // Skip the parameter name
                        skip_param(&mut chars);
                    } else if ch == '/' && chars.peek().is_none() {
                        // Skip trailing slash (it's ignored in equality)
                        break;
                    } else {
                        // Hash regular character
                        ch.hash(state);
                    }
                }
            }
        }
    }
}

/// Id of a path stored in the [`PathArena`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PathId(u32);

/// Slot in the path arena.
#[derive(Debug, Clone, PartialEq)]
struct Slot {
    /// Id of the interned part string.
    /// None represents PathPart::Empty.
    part_id: Option<string_interner::DefaultSymbol>,

    /// Id of the prefix already stored in the arena.
    prefix_id: Option<PathId>,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct PathArena {
    strings: string_interner::DefaultStringInterner,
    data: Vec<Slot>,
}

impl PathArena {
    /// Create a new path arena
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert a new part of the path into the arena.
    ///
    /// Returns the id of the path with inserted part as it's tail.
    /// This id can be used as a `prefix_id` in further insertions.
    pub fn insert(&mut self, prefix_id: Option<PathId>, part: PathPart) -> PathId {
        let part_id = match part {
            PathPart::Empty => None,
            PathPart::Segment(s) => Some(self.strings.get_or_intern(s)),
        };
        let path_id = self.data.len();
        self.data.push(Slot { part_id, prefix_id });

        PathId(path_id as u32)
    }

    /// Get path with specified id.
    pub fn get<'a>(&'a self, id: PathId) -> Path<'a> {
        let slot = &self.data[id.0 as usize];

        let part = match slot.part_id {
            None => PathPart::Empty,
            Some(s) => PathPart::Segment(
                self.strings
                    .resolve(s)
                    .expect("inserted path should be resolvable"),
            ),
        };

        Path {
            arena: self,
            id,
            part,
            prefix_id: slot.prefix_id,
        }
    }
}

/// Helper function that checks that non empty path is valid and reports errors.
/// The function should not be called on empty paths.
///
/// - `path` should be a path string that went through [`text::parse_string`](crate::text::parse_string).
/// - `text_range` should be the range of the token returned by
///   [`Path::string`](better_api_syntax::ast::Path::string).
///
/// NOTE: Currently known issue is, that if path contains escape chars (like `\n`, `\t`), ranges
/// reported in the diagnostics will be off by one, because they don't take into account that one
/// char (ie new line) is actually two chars in the user editor (ie `\n`). Since path doesn't allow
/// any of the characters you get while escaping, this is fine for know. User will get an error
/// about using invalid characters, which is reported with correct range because we take the range
/// of the whole path node. When they fix this error, other potential errors are also reported
/// correctly. Additionally error is always reported with range inside of the path, so it's not the
/// end of the world to not handle this edge case initially.
fn validate_path(path: &str, text_range: TextRange, diagnostics: &mut Vec<Report>) {
    assert!(
        !path.is_empty() && path != "/",
        "validate_path should not be called on an empty path"
    );

    let mut chars = path.char_indices().peekable();

    // Check leading character
    if !matches!(chars.peek(), Some((_, '/'))) {
        diagnostics.push(
            Report::error("missing leading `/` in path".to_string())
                .add_label(Label::primary(
                    "missing leading `/` in path".to_string(),
                    text_range.into(),
                ))
                .with_note("help: path has to be absolute and start with a `/`".to_string()),
        );
    }

    // Is current character after slash. Used to validate if parameter takes up the whole segment.
    let mut after_slash = false;
    let mut all_chars_valid = true;
    let mut all_escapes_valid = true;
    while let Some((idx, ch)) = chars.next() {
        match ch {
            '/' => {
                // Check for empty segments
                if chars.peek().is_some_and(|(_, ch)| *ch == '/') {
                    // +1 accounts for the starting `"` of the string containing the path.
                    let segment_start = Into::<usize>::into(text_range.start()) + idx + 1;

                    diagnostics.push(
                        Report::error("path contains empty segments".to_string())
                            .add_label(Label::primary(
                                "path contains empty segments".to_string(),
                                text_range.into(),
                            ))
                            .add_label(Label::secondary(
                                "empty segment".to_string(),
                                Span::new(segment_start, segment_start + 2), // utf8 length of '//' is 2
                            ))
                            .with_note(
                                "help: path must not contain empty segments `//`".to_string(),
                            ),
                    );
                }

                // Check trailing `/`
                if chars.peek().is_none() {
                    diagnostics.push(
                        Report::warning("unnecessary trailing `/` in path".to_string())
                            .add_label(Label::primary(
                                "unnecessary trailing `/` in path".to_string(),
                                text_range.into(),
                            ))
                            .with_note(
                                "help: trailing `/` is unnecessary and is ignored".to_string(),
                            ),
                    );
                }
            }

            // Validate parameters
            '{' => validate_path_param(path, &mut chars, after_slash, idx, text_range, diagnostics),

            // Check escaped By RFC 3986 valid escape is "%" HEXDIG HEXDIG
            // where HEXDIG is 0-9 and a-f or A-F.
            '%' => {
                let first = chars.next();
                let second = chars.next();

                let valid = first.is_some_and(|(_, c)| c.is_ascii_hexdigit())
                    && second.is_some_and(|(_, c)| c.is_ascii_hexdigit());

                if !valid {
                    all_escapes_valid = false;
                }
            }

            // What is left is to check char is valid. Branches under here do that.
            // Check that character is valid. By RFC 3986 valid absolute paths are one of:
            // ```text
            // ALPHA / DIGIT / "-" / "." / "_" / "~"  / ":" / "@" / "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
            // ```

            // alpha numeric is valid
            ch if ch.is_ascii_alphanumeric() => (),

            // Valid special "special" chars
            '-' | '.' | '_' | '~' | ':' | '@' | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+'
            | ',' | ';' | '=' => (),

            // Other chars are invalid.
            _ => {
                all_chars_valid = false;
            }
        }

        after_slash = ch == '/';
    }

    if !all_chars_valid {
        diagnostics.push(
            Report::error("path contains invalid characters".to_string()).add_label(
                Label::primary(
                    "path contains invalid characters".to_string(),
                    text_range.into(),
                ),
            ).with_note("help: path has to be an absolute path that can contain parameters `{param_name}`. See RFC 3986 for what valid path characters are".to_string()),
        );
    }

    if !all_escapes_valid {
        diagnostics.push(
            Report::error("path contains invalid escape sequences".to_string()).add_label(
                Label::primary(
                    "path contains invalid escape sequences".to_string(),
                    text_range.into(),
                ),
            ).with_note("help: valid escape sequence in path is `% HEXDIG HEXDIG` where `HEXDIG` is hexadecimal digit.".to_string()),
        );
    }
}

/// Validates path parameter is valid.
///
/// - `path` is the string of the whole path.
/// - `chars` is iterator over path that starts (chars.next() returns) character after beginning `{}`
/// - `start_idx` is index of starting `{` - `&path[start_idx..(start_idx+1)]` should be `{`
/// - `text_range` is the range of the whole path in the syntax tree
fn validate_path_param(
    path: &str,
    chars: &mut Peekable<impl Iterator<Item = (usize, char)>>,
    after_slash: bool,
    start_idx: usize,
    text_range: TextRange,
    diagnostics: &mut Vec<Report>,
) {
    // End index is char after '}' so that `&path[start_idx..end_idx]` gives the full param
    let end_idx = chars.find_map(|(idx, ch)| if ch == '}' { Some(idx + 1) } else { None });

    // +1 accounts for the starting `"` of the string containing the path.
    let token_start = Into::<usize>::into(text_range.start()) + 1;
    let param_start = token_start + start_idx;

    if let Some(end_idx) = end_idx {
        let param_end = token_start + end_idx;

        // Check parameter has a valid position
        // Parameter has invalid position if there isn't a `/` before or after it
        if !after_slash || chars.peek().is_some_and(|(_, ch)| *ch != '/') {
            diagnostics.push(
                    Report::error("invalid partial path parameter".to_string()).add_label(
                        Label::primary(
                            "invalid partial path parameter".to_string(),
                            Span::new(param_start, param_end),
                        ),
                ).with_note("help: path parameter has to take up a whole path segment, not just a part of it.\n      That is `/{param}` is valid and `/pre{param}after` is not".to_string()),
            );
        }

        // Check name of parameter is not empty and is valid
        let param_name = &path[(start_idx + 1)..(end_idx - 1)];
        if param_name.is_empty() {
            diagnostics.push(
                Report::error("empty path parameter".to_string())
                    .add_label(Label::primary(
                        "empty path parameter".to_string(),
                        Span::new(param_start, param_end),
                    ))
                    .with_note("help: path parameter has to have a name".to_string()),
            );
        } else if !is_param_name_valid(param_name) {
            diagnostics.push(
                Report::error("invalid path parameter name".to_string())
                    .add_label(Label::primary(
                        "invalid path parameter name".to_string(),
                        Span::new(param_start, param_end),
                    ))
                    .with_note("help: name can only contain alphanumeric characters and `_`.  It also has to start with alphabetic character.".to_string()),
            );
        }
    } else {
        // Report parameter not ending
        diagnostics.push(
            Report::error("missing ending `}` in path parameter".to_string())
                .add_label(Label::primary(
                    "missing ending `}` in path parameter".to_string(),
                    text_range.into(),
                ))
                .add_label(Label::secondary(
                    "path parameter starting here".to_string(),
                    Span::new(param_start, param_start + 1), // utf8 len of '}' is 1
                )),
        );
    }
}

/// Returns if parameter name is valid identifier.
fn is_param_name_valid(name: &str) -> bool {
    name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
        && name.chars().next().is_some_and(|c| c.is_ascii_alphabetic())
}

#[cfg(test)]
mod test {
    use better_api_syntax::{TextRange, TextSize, parse, tokenize};

    use crate::{path::validate_path, text::parse_string};

    use super::PathPart;

    /// Helper function for constructing mock text range that belongs to a string.
    fn text_range_for_str(s: &str) -> TextRange {
        // In real world range contains start and end `"` of a path string, which is why range has
        // to be +2
        TextRange::new(TextSize::new(0), TextSize::new(s.len() as u32 + 2))
    }

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
    fn path_part_ordering_trailing_slash_ignored() {
        assert_eq!(PathPart::Segment("/foo"), PathPart::Segment("/foo/"));
        assert_eq!(
            PathPart::Segment("/foo/bar"),
            PathPart::Segment("/foo/bar/")
        );
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
    fn hash_trailing_slash_ignored() {
        // Trailing slash should be ignored in hash
        let part1 = PathPart::Segment("/foo");
        let part2 = PathPart::Segment("/foo/");

        assert_eq!(hash_pathpart(&part1), hash_pathpart(&part2));
    }

    #[test]
    fn hash_trailing_slash_with_params() {
        let part1 = PathPart::Segment("/foo/{id}");
        let part2 = PathPart::Segment("/foo/{name}/");

        assert_eq!(hash_pathpart(&part1), hash_pathpart(&part2));
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
            (PathPart::Segment("/foo/"), PathPart::Segment("/foo")),
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
}
