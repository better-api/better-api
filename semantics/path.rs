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

use better_api_diagnostic::{Label, Report, Span};
use better_api_syntax::TextRange;

/// A single part of the [`Path`].
///
/// See [module documentation](self) for more details.
#[derive(Debug)]
#[repr(transparent)]
pub struct PathPart(str);

/// Represents path inside a URL.
///
/// See [module documentation](self) for more details.
pub struct Path<'a> {
    arena: &'a PathArena,

    /// Tail part of the path
    pub part: &'a PathPart,

    /// Id of the prefix
    pub prefix_id: Option<PathId>,
}

impl<'a> Path<'a> {
    /// Get prefix of the path.
    pub fn prefix(&'a self) -> Option<Path<'a>> {
        self.prefix_id.map(|id| self.arena.get(id))
    }
}

impl PathPart {
    /// Parses string into path part.
    ///
    /// This function checks that string is valid and reports the possible invalid characters
    /// or path parameters.
    pub fn new<'a>(
        string: &'a str,
        text_range: TextRange,
        diagnostics: &mut Vec<Report>,
    ) -> &'a PathPart {
        validate_path(string, text_range, diagnostics);
        Self::from_str(string)
    }

    /// Get `&str` representation
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Cast &str to PathPart, without checking if it's valid
    ///
    /// Safety: Casting a string that is not a valid PathPart into one,
    /// can lead to undefined behavior.
    fn from_str(string: &str) -> &PathPart {
        // Safety: PathPart is a semantic wrapper around str,
        // with same layout and alignment so pointer cast is safe.
        unsafe { &*(string as *const str as *const PathPart) }
    }
}

impl AsRef<str> for PathPart {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

/// Id of a path stored in the [`PathArena`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PathId(u32);

/// Slot in the path arena.
#[derive(Debug, Clone, PartialEq)]
struct Slot {
    /// Id of the interned part string.
    part_id: string_interner::DefaultSymbol,

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
    pub fn insert(&mut self, prefix_id: Option<PathId>, part: &PathPart) -> PathId {
        let part_id = self.strings.get_or_intern(part);
        let path_id = self.data.len();
        self.data.push(Slot { part_id, prefix_id });

        PathId(path_id as u32)
    }

    /// Get path with specified id.
    pub fn get<'a>(&'a self, id: PathId) -> Path<'a> {
        let slot = &self.data[id.0 as usize];

        let part_str = self
            .strings
            .resolve(slot.part_id)
            .expect("inserted path should be resolvable");

        let part = PathPart::from_str(part_str);

        Path {
            arena: self,
            part,
            prefix_id: slot.prefix_id,
        }
    }
}

fn validate_path(path: &str, text_range: TextRange, diagnostics: &mut Vec<Report>) {
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
    while let Some((idx, ch)) = chars.next() {
        // Check for empty segments
        if ch == '/' && matches!(chars.peek(), Some((_, '/'))) {
            let segment_start = Into::<usize>::into(text_range.start()) + idx;

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
                    .with_note("help: path must not contain empty segments `//`".to_string()),
            );
        }

        // Check trailing `/`
        if ch == '/' && chars.peek().is_none() {
            diagnostics.push(
                Report::warning("unnecessary trailing `/` in path".to_string())
                    .add_label(Label::primary(
                        "unnecessary trailing `/` in path".to_string(),
                        text_range.into(),
                    ))
                    .with_note("help: trailing `/` is unnecessary and is ignored".to_string()),
            );
        }

        // Validate parameters
        if ch == '{' {
            let end_idx = validate_path_param(&mut chars, idx, text_range, diagnostics);
            let param_start = Into::<usize>::into(text_range.start()) + idx;

            // Check parameter ends
            if end_idx.is_none() {
                diagnostics.push(
                    Report::error("missing ending `}` in path parameter".to_string())
                        .add_label(Label::primary(
                            "missing ending `}` in path parameter".to_string(),
                            text_range.into(),
                        ))
                        .add_label(Label::secondary(
                            "path parameter starting here".to_string(),
                            Span::new(param_start, param_start + 1),
                        )),
                );
            }

            // Check parameter has a valid position
            // Parameter has invalid position if there isn't a `/` before or after it
            let invalid_position = !after_slash || chars.peek().is_some_and(|(_, ch)| *ch != '/');
            if let Some(end_idx) = end_idx
                && invalid_position
            {
                let param_end = Into::<usize>::into(text_range.start()) + end_idx;

                diagnostics.push(
                    Report::error("invalid partial path parameter".to_string()).add_label(
                        Label::primary(
                            "invalid partial path parameter position".to_string(),
                            Span::new(param_start, param_end),
                        ),
                    ).with_note("help: path parameter has to take up a whole path segment, not just a part of it".to_string()),
                );
            }
        }

        // TODO: check invalid chars (do not forget url encoding)

        after_slash = ch == '/';
    }
}

fn validate_path_param(
    chars: &mut impl Iterator<Item = (usize, char)>,
    start_idx: usize,
    text_range: TextRange,
    diagnostics: &mut Vec<Report>,
) -> Option<usize> {
    while let Some((idx, ch)) = chars.next() {
        // Check if the parameter is ended
        if ch == '}' {
            return Some(idx + 1); // utf8 size of `}` is 1
        }

        // TODO: Implement char is identifier
    }

    None
}
