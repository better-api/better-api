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
    pub fn as_mime(&self) -> &Mime {
        &self.0
    }

    /// Get the top level media type for this Mime.
    pub fn type_(&self) -> mime::Name {
        self.0.type_()
    }

    /// Get the subtype of this Mime.
    pub fn subtype(&self) -> mime::Name {
        self.0.subtype()
    }

    /// Get an optional +suffix for this Mime.
    pub fn suffix(&self) -> Option<mime::Name> {
        self.0.suffix()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub(crate) struct MimeId(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub(crate) struct ConcreteMimeId(u32);

pub(crate) struct MimeTypesBuilder<'a> {
    arena: &'a mut MimeArena,
    start: u32,
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

impl MimeArena {}
