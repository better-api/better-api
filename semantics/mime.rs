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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
#[derive(derive_more::Debug, Clone)]
pub struct MimeRange<'a> {
    #[debug(skip)]
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

#[derive(Debug, Clone, PartialEq)]
enum Slot {
    Mime(Mime),
    ConcreteMime(ConcreteMime),
}

#[derive(Debug, Default, Clone, PartialEq)]
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

#[cfg(test)]
mod test {
    use super::*;

    fn parse_mime(value: &str) -> Mime {
        value.parse().expect("mime should parse")
    }

    #[test]
    fn concrete_mime_try_from_accepts_only_non_wildcards() {
        let json = parse_mime("application/json");
        let problem_json = parse_mime("application/problem+json");

        let concrete_json = ConcreteMime::try_from(json.clone())
            .unwrap_or_else(|_| panic!("json should be concrete"));
        let concrete_problem_json = ConcreteMime::try_from(problem_json.clone())
            .unwrap_or_else(|_| panic!("problem json should be concrete"));

        assert_eq!(concrete_json.as_mime(), &json);
        assert_eq!(concrete_json.type_(), mime::APPLICATION);
        assert_eq!(concrete_json.subtype(), mime::JSON);
        assert_eq!(concrete_json.suffix(), None);

        assert_eq!(concrete_problem_json.as_mime(), &problem_json);
        assert_eq!(concrete_problem_json.type_(), mime::APPLICATION);
        assert_eq!(concrete_problem_json.subtype(), "problem");
        assert_eq!(concrete_problem_json.suffix(), Some(mime::JSON));

        assert!(ConcreteMime::try_from(parse_mime("image/*")).is_err());
        assert!(ConcreteMime::try_from(parse_mime("*/*")).is_err());
    }

    #[test]
    fn arena_stores_mime_ranges_and_concrete_mimes() {
        let mut arena = MimeArena::default();

        let image_any = parse_mime("image/*");
        let text_plain = parse_mime("text/plain");
        let image_png = parse_mime("image/png");

        let concrete_json = ConcreteMime::try_from(parse_mime("application/json"))
            .unwrap_or_else(|_| panic!("json should be concrete"));
        let concrete_problem_json = ConcreteMime::try_from(parse_mime("application/problem+json"))
            .unwrap_or_else(|_| panic!("problem json should be concrete"));

        let single_range = arena.add_mime(image_any.clone());
        let json_id = arena.add_concrete_mime(concrete_json.clone());

        let mut builder = arena.start_range();
        builder.add(text_plain.clone());
        builder.add(image_png.clone());
        let mixed_range = builder.finish();

        let problem_json_id = arena.add_concrete_mime(concrete_problem_json.clone());

        assert_eq!(single_range, MimeRangeId { start: 0, end: 1 });
        assert_eq!(json_id, ConcreteMimeId(1));
        assert_eq!(mixed_range, MimeRangeId { start: 2, end: 4 });
        assert_eq!(problem_json_id, ConcreteMimeId(4));

        assert_eq!(
            arena
                .get_mime_range(single_range)
                .cloned()
                .collect::<Vec<_>>(),
            vec![image_any]
        );

        assert_eq!(
            arena
                .get_mime_range(mixed_range)
                .cloned()
                .collect::<Vec<_>>(),
            vec![text_plain, image_png],
        );

        assert_eq!(
            arena.data[json_id.0 as usize],
            Slot::ConcreteMime(concrete_json)
        );
        assert_eq!(
            arena.data[problem_json_id.0 as usize],
            Slot::ConcreteMime(concrete_problem_json)
        );
    }
}
