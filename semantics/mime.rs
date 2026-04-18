//! Module for working with mime types.
//!
//! The main types in the module are [`Mime`], [`ConcreteMime`] and [`MimeRange`].
//!
//! Type [`Mime`] is a basic mime type that can be anything, including wild cards.
//! For example, the following are all valid [`Mime`]s:
//!
//! - `application/json`
//! - `application/problem+json`
//! - `image/png`
//! - `image/*`
//! - `*/*`
//!
//! A [`ConcreteMime`] type is a [`Mime`] that is not a wild card. For instance
//! `image/png` is a valid [`ConcreteMime`], but `image/*` isn't.
//!
//! A [`MimeRange`] is an array (range) of [`Mime`]s.

pub use mime::Mime;

/// A mime type without any wild cards.
#[derive(Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ConcreteMime(Mime);

/// Error representing a mime type contains wild cards when it shouldn't contain any.
pub struct NotConcreteError;

impl TryFrom<Mime> for ConcreteMime {
    type Error = NotConcreteError;

    fn try_from(value: Mime) -> Result<Self, Self::Error> {
        if value.type_() == mime::STAR || value.subtype() == mime::STAR {
            Err(NotConcreteError)
        } else {
            Ok(ConcreteMime(value))
        }
    }
}

impl ConcreteMime {
    /// Return the concrete mime as a normal [Mime] type.
    pub fn as_mime(&self) -> &Mime {
        &self.0
    }

    /// Get the top level media type for this Mime.
    pub fn type_<'a>(&'a self) -> mime::Name<'a> {
        self.0.type_()
    }

    /// Get the subtype of this Mime.
    pub fn subtype<'a>(&'a self) -> mime::Name<'a> {
        self.0.subtype()
    }

    /// Get an optional +suffix for this Mime.
    pub fn suffix<'a>(&'a self) -> Option<mime::Name<'a>> {
        self.0.suffix()
    }
}

/// Id representing a range of [Mime] types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) struct MimeRangeId {
    start: u32,
    end: u32,
}

/// Id of a [ConcreteMime] type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub(crate) struct ConcreteMimeId(u32);

/// Helper type for adding an array (range) of mime types to arena.
pub(crate) struct MimeRangeBuilder<'a> {
    data: &'a mut Vec<Slot>,
    start: u32,
    finished: bool,
}

impl<'a> MimeRangeBuilder<'a> {
    fn new(data: &'a mut Vec<Slot>) -> Self {
        let start = data.len() as u32;

        Self {
            data,
            start,
            finished: false,
        }
    }

    /// Add a mime type to the range
    pub(crate) fn add(&mut self, mime: Mime) {
        self.data.push(Slot::Mime(mime));
    }

    /// Finish building the range.
    pub(crate) fn finish(mut self) -> MimeRangeId {
        self.finished = true;

        let end = self.data.len() as u32;
        MimeRangeId {
            start: self.start,
            end,
        }
    }
}

impl<'a> Drop for MimeRangeBuilder<'a> {
    fn drop(&mut self) {
        if self.finished {
            return;
        }

        self.data.truncate(self.start as usize);
    }
}

/// Iterator over [`Mime`] types.
#[derive(Clone)]
pub struct MimeRange<'a> {
    arena: &'a MimeArena,

    next: u32,
    end: u32,
}

impl<'a> Iterator for MimeRange<'a> {
    type Item = &'a Mime;

    fn next(&mut self) -> Option<Self::Item> {
        if self.next >= self.end {
            return None;
        }

        let mime = match &self.arena.data[self.next as usize] {
            Slot::Mime(mime) => mime,
            Slot::ConcreteMime(_) => unreachable!("mime range must contain only mime types"),
        };

        self.next += 1;
        Some(mime)
    }
}

#[derive(Clone, PartialEq)]
enum Slot {
    Mime(Mime),
    ConcreteMime(ConcreteMime),
}

#[derive(Default, Clone, PartialEq)]
pub(crate) struct MimeArena {
    data: Vec<Slot>,
}

impl MimeArena {
    /// Start building a new range of mime types.
    pub(crate) fn start_range<'a>(&'a mut self) -> MimeRangeBuilder<'a> {
        MimeRangeBuilder::new(&mut self.data)
    }

    /// Create a range of mimes with a single mime type.
    ///
    /// This is equivalent to calling [Self::start_range] and adding only
    /// one mime type.
    pub(crate) fn add_mime(&mut self, mime: Mime) -> MimeRangeId {
        let mut builder = self.start_range();
        builder.add(mime);
        builder.finish()
    }

    /// Add a concrete mime type to the arena.
    pub(crate) fn add_concrete_mime(&mut self, mime: ConcreteMime) -> ConcreteMimeId {
        let idx = self.data.len();
        self.data.push(Slot::ConcreteMime(mime));
        ConcreteMimeId(idx as u32)
    }

    /// Get iterator over mime range
    pub(crate) fn get_mime_range<'a>(&'a self, id: MimeRangeId) -> MimeRange<'a> {
        MimeRange {
            arena: self,
            next: id.start,
            end: id.end,
        }
    }
}
