//! Module for working with strings.
//!
//! Most of the code in this module is crate private, since downstream users have a nicer API
//! exposed via [`spec::view`](crate::spec::view) module. However, some of the types in this module
//! are used in public API.

use std::borrow::Cow;

use better_api_diagnostic::{Label, Report, Span};
use better_api_syntax::{TextRange, ast};

#[cfg(test)]
mod test;

/// Id of an interned string.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) struct StringId(string_interner::DefaultSymbol);

impl From<NameId> for StringId {
    fn from(value: NameId) -> Self {
        value.0
    }
}

/// Represents a name.
///
/// Name is a string made only of ASCII alphanumeric characters, `_`, `-` and `.`.
/// Additionally it has to start with an ASCII alphabetic character, which also means
/// it's at least one long.
#[derive(Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct Name(str);

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl Name {
    /// Creates a new name from &str.
    ///
    /// Safety: Caller is responsible for validating that string is a valid name.
    unsafe fn from_str_unchecked(s: &str) -> &Self {
        // Safety: Name is transparent repr of str, so this operation is safe
        unsafe { &*(s as *const str as *const Name) }
    }

    /// Returns string representation of the name
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Returns identifier representation of the name.
    ///
    /// Identifier is gotten by replacing `-` and `.` with `_`.
    pub fn as_identifier(&self) -> Cow<'_, str> {
        if !self.0.contains('.') && !self.0.contains('-') {
            Cow::Borrowed(self.as_str())
        } else {
            let owned = self.as_str().to_owned().replace(['.', '-'], "_");
            Cow::Owned(owned)
        }
    }
}

/// Id of an interned name.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub(crate) struct NameId(StringId);

impl NameId {
    #[cfg(test)]
    pub(crate) unsafe fn from_string_id(id: StringId) -> Self {
        Self(id)
    }
}

#[derive(Default, Clone)]
pub(crate) struct StringInterner(string_interner::DefaultStringInterner);

impl StringInterner {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Default::default()
    }

    /// Interns a given string
    pub(crate) fn get_or_intern(&mut self, string: impl AsRef<str>) -> StringId {
        let id = self.0.get_or_intern(string);
        StringId(id)
    }

    /// Return symbol for given string, if any
    pub(crate) fn get(&self, string: impl AsRef<str>) -> Option<StringId> {
        self.0.get(string).map(StringId)
    }

    /// Returns the string for given symbol
    pub(crate) fn resolve(&self, id: StringId) -> &str {
        self.0
            .resolve(id.0)
            .expect("interend string should be resolvable")
    }

    pub(crate) fn resolve_name(&self, id: NameId) -> &Name {
        let name = self.resolve(id.0);

        // Safety: interner must contain a valid name for NameId.
        unsafe { Name::from_str_unchecked(name) }
    }

    /// Lowers name by parsing, validating and interning it.
    ///
    /// If `reports` is `Some`, parsing/validation reports are emitted into it.
    /// If `reports` is `None`, reports are suppressed.
    ///
    /// Returns interned string id of the name if it's valid.
    pub(crate) fn lower_name(
        &mut self,
        name: &ast::Name,
        mut reports: Option<&mut Vec<Report>>,
    ) -> Option<NameId> {
        let token = name.token();

        let name_str: Cow<_> = match &token {
            ast::NameToken::Identifier(ident) => ident.text().into(),
            ast::NameToken::String(string) => parse_string(string, reports.as_deref_mut()),
        };

        if let Err(report) = validate_name(&name_str, token.text_range()) {
            maybe_push_report(&mut reports, report);
            return None;
        }

        let name_id = self.get_or_intern(name_str);
        Some(NameId(name_id))
    }
}

/// Validates if given string is a valid name.
pub(crate) fn validate_name(name: &str, range: TextRange) -> Result<&Name, Report> {
    let is_valid = name
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-' || c == '.')
        && name.chars().next().is_some_and(|c| c.is_ascii_alphabetic());

    if is_valid {
        // Safety: We have checked it's a valid name.
        let name = unsafe { Name::from_str_unchecked(name) };
        Ok(name)
    } else {
        Err(Report::error("invalid name".to_string()).add_label(Label::primary("invalid name".to_string(), range.into())).with_note(
                "help: name can only contain alphanumeric characters, `_`, `-` and `.`. It also has to start with alphabetic character.".to_string(),
            ))
    }
}

/// Parses a syntax token that represents a string.
///
/// This removes leading and trailing `"` and escapes the character. Returned
/// `Cow<'_, str>` is the rust representation of the string itself.
///
/// If `reports` is `Some`, invalid escape reports are emitted into it.
/// If `reports` is `None`, reports are suppressed.
pub(crate) fn parse_string<'a>(
    token: &'a ast::StringToken,
    mut reports: Option<&mut Vec<Report>>,
) -> Cow<'a, str> {
    let mut text = token.text();

    assert!(
        &text[0..1] == "\"" && &text[text.len() - 1..text.len()] == "\"",
        "invalid string token"
    );
    text = &text[1..text.len() - 1]; // Remove start and end `"`

    if !text.contains('\\') {
        return Cow::Borrowed(text);
    }

    // +1 accounts for the starting `"`.
    let token_start: usize = Into::<usize>::into(token.text_range().start()) + 1;
    let mut res = String::new();
    let mut chars = text.char_indices();

    while let Some((idx, ch)) = chars.next() {
        if ch != '\\' {
            res.push(ch);
            continue;
        }

        let Some((end, esc)) = chars.next() else {
            maybe_push_report(
                &mut reports,
                Report::error("expected escaped character, got '\"'".to_string()).add_label(
                    Label::primary(
                        "expected escaped character".to_string(),
                        Span::new(token_start + idx, token_start + idx + 1),
                    ),
                ),
            );
            break;
        };

        match esc {
            'n' => res.push('\n'),
            't' => res.push('\t'),
            '"' => res.push('"'),
            '\\' => res.push('\\'),
            _ => {
                maybe_push_report(
                    &mut reports,
                    Report::error(format!("got invalid escape character `{esc}`")).add_label(
                        Label::primary(
                            "invalid escape character".to_string(),
                            Span::new(token_start + idx, token_start + end + esc.len_utf8()),
                        ),
                    ),
                );
                res.push(esc);
            }
        }
    }

    Cow::Owned(res)
}

fn maybe_push_report(reports: &mut Option<&mut Vec<Report>>, report: Report) {
    if let Some(reports) = reports.as_mut() {
        reports.push(report);
    }
}
