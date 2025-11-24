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

use better_api_diagnostic::Report;

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
    pub fn new<'a>(string: &'a str, _diagnostics: &mut Vec<Report>) -> &'a PathPart {
        // TODO: Check string is valid path part

        // Safety: We checked that string is valid path part.
        unsafe { Self::from_str_unchecked(string) }
    }

    /// Get `&str` representation
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Cast &str to PathPart, without checking if it's valid
    ///
    /// Safety: Casting a string that is not a valid PathPart into one,
    /// can lead to undefined behavior.
    unsafe fn from_str_unchecked(string: &str) -> &PathPart {
        // Safety: PathPart is a wrapper around str, so pointer cast is safe.
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

        // Safety: The only way to construct PathId outside of this file is to insert
        // a valid &PathPart into PathArena. Therefore queried data is valid.
        let part = unsafe { PathPart::from_str_unchecked(part_str) };

        Path {
            arena: self,
            part,
            prefix_id: slot.prefix_id,
        }
    }
}
