//! Defines types and functions for working with names.
//!
//! Syntactically name can be either a string or an identifier, depending
//! on context and user preference.
//!
//! In the following snippet name is identifier for type.
//! ```text
//! type Foo: string
//! ```
//!
//! Here name is the name of the field, and is sometimes string and sometimes an identifier.
//! ```text
//! type Example: rec {
//!     identName: string
//!     "string-name": string
//! }
//! ```
//!
//! Name has some constraints on what characters it can use, since it can be used in things
//! like HTTP headers and query parameters. Therefore characters inside name are limited
//! to alphanumerical, `_`, `-` and `.`.

use std::borrow::Borrow;
use std::ops::Deref;

use better_api_diagnostic::Report;

/// An owned semantic name.
///
/// It also implements [`Deref`] to [`Name`], meaning that all methods on
/// [`Name`] slices are available on [`NameBuf`] values as well.
///
/// More details about what a valid name is can be found in
/// the [module documentation](self).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NameBuf(Box<str>);

impl NameBuf {
    pub fn new(s: &str) -> Result<Self, Report> {
        let name = Name::new(s)?;
        Ok(name.to_owned())
    }
}

impl Deref for NameBuf {
    type Target = Name;

    fn deref(&self) -> &Self::Target {
        // Safety: Name is a transparent wrapper around str, therefore lifetimes for &self and
        // &Name are equal. Additionally &Name and &NameBuf have the same semantics (what is a
        // valid name).
        unsafe { &*(self.0.deref() as *const str as *const Name) }
    }
}

impl Borrow<Name> for NameBuf {
    fn borrow(&self) -> &Name {
        self.deref()
    }
}

/// A slice of semantic name.
///
/// This is an _unsized_ type, meaning that it must always be used behind a
/// pointer like `&` or [`Box`]. For an owned version of this type,
/// see [`NameBuf`].
///
/// This type is to [`NameBuf`] what [`Path`](std::path::Path) is to
/// [`PathBuf`](std::path::PathBuf).
///
/// More details about what a valid name is can be found in
/// the [module documentation](self).
#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Name(str);

impl Name {
    pub fn new(s: &str) -> Result<&Self, Report> {
        let is_valid = s
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-' || c == '.')
            && s.chars().next().is_some_and(|c| c.is_ascii_alphabetic());

        if is_valid {
            // Safety: we checked that it's valid
            let name = unsafe { Name::from_str_unchecked(s) };
            Ok(name)
        } else {
            Err(Report::error("invalid name".to_string()).with_note(
                "help: name can only contain alphanumeric characters, `_`, `-` and `.`. It also has to start with alphabetic character.".to_string(),
            ))
        }
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    unsafe fn from_str_unchecked(s: &str) -> &Self {
        unsafe { &*(s as *const str as *const Self) }
    }
}

impl ToOwned for Name {
    type Owned = NameBuf;

    fn to_owned(&self) -> Self::Owned {
        NameBuf(Box::from(self.as_str()))
    }
}

#[cfg(test)]
mod test {
    use crate::name::{Name, NameBuf};

    #[test]
    fn valid_name() {
        let res = Name::new("this-is-a.valid_name123").unwrap();
        assert_eq!(res.as_str(), "this-is-a.valid_name123");
    }

    #[test]
    fn invalid_name() {
        let res = Name::new("invalid name");
        assert!(res.is_err());
    }

    #[test]
    fn owned_name() {
        let res = NameBuf::new("validName").unwrap();
        assert_eq!(res.as_str(), "validName");
    }
}
