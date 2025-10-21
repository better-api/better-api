//! Defines types for converting [string tokens](better_api_syntax::Kind::TOKEN_STRING)
//! into [`String`] and `&str`.
//
// Working with string is split into two parts.
// 1. Walk the AST and build a cache of string token -> String.
// 2. Get string from token.
//
// During building of the cache errors for invalid strings are reported.
// This errors are mostly around invalid escape sequences.

use std::collections::HashMap;
use std::ops::Range;

use better_api_diagnostic::{Label, Report, Span};
use better_api_syntax::ast::AstNode;
use better_api_syntax::{Kind, SyntaxToken, ast};
use better_api_syntax::{NodeOrToken, TextRange};

/// Cache of all the strings in the AST.
#[derive(Debug)]
pub struct StringCache {
    data: String,
    ranges: HashMap<TextRange, Range<usize>>,
}

impl StringCache {
    /// Creates a new StringCache from the root note.
    pub fn new(root: &ast::Root, diagnostics: &mut Vec<Report>) -> Self {
        let mut cache = Self {
            data: Default::default(),
            ranges: Default::default(),
        };

        for node in root.syntax().descendants_with_tokens() {
            match node {
                NodeOrToken::Node(_) => (),
                NodeOrToken::Token(t) => match t.kind() {
                    Kind::TOKEN_STRING => cache.add_string(t, diagnostics),
                    Kind::TOKEN_IDENTIFIER => cache.add_ident(t),
                    _ => (),
                },
            }
        }

        cache
    }

    /// Get string for a specific token.
    ///
    /// Syntax token should be a TOKEN_STRING, otherwise None is returned.
    pub fn get(&self, token: &SyntaxToken) -> Option<&str> {
        if !matches!(token.kind(), Kind::TOKEN_STRING | Kind::TOKEN_IDENTIFIER) {
            return None;
        }

        let range = self.ranges.get(&token.text_range())?;
        Some(&self.data[range.clone()])
    }

    fn add_ident(&mut self, token: SyntaxToken) {
        let start = self.data.len();
        self.data.push_str(token.text());
        self.ranges.insert(
            token.text_range(),
            Range {
                start,
                end: self.data.len(),
            },
        );
    }

    fn add_string(&mut self, token: SyntaxToken, diagnostics: &mut Vec<Report>) {
        let mut text = token.text();
        debug_assert!(&text[0..1] == "\"" && &text[text.len() - 1..text.len()] == "\"");
        text = &text[1..text.len() - 1]; // Remove start and end `"`

        let token_start: usize = token.text_range().start().into();
        let start = self.data.len();

        let mut chars = text.char_indices();

        while let Some((idx, ch)) = chars.next() {
            if ch != '\\' {
                self.data.push(ch);
                continue;
            }

            let Some((end, esc)) = chars.next() else {
                diagnostics.push(
                    Report::error("expected escaped character, got '\"'".to_string()).with_label(
                        Label::new(
                            "expected escaped character".to_string(),
                            Span::new(token_start + idx, token_start + idx + 1),
                        ),
                    ),
                );
                break;
            };

            match esc {
                'n' => self.data.push('\n'),
                't' => self.data.push('\t'),
                '"' => self.data.push('"'),
                '\\' => self.data.push('\\'),
                _ => {
                    diagnostics.push(
                        Report::error(format!("got invalid escape character `{esc}`")).with_label(
                            Label::new(
                                "invalid escape character".to_string(),
                                Span::new(token_start + idx, token_start + end),
                            ),
                        ),
                    );
                    self.data.push(esc);
                }
            }
        }

        self.ranges.insert(
            token.text_range(),
            Range {
                start,
                end: self.data.len(),
            },
        );
    }
}

#[cfg(test)]
mod test {
    use std::ops::Range;

    use indoc::indoc;

    use better_api_syntax::TextRange;
    use better_api_syntax::{parse, tokenize};

    use crate::text::StringCache;

    #[test]
    fn new_cache() {
        let text = indoc! {r#"
            name: "foo"
            name: "invalid\"
            name: "inval\id"
            name: "valid \t \n \\ \" foo"
        "#};

        let mut diagnostics = vec![];
        let tokens = tokenize(text, &mut diagnostics);
        let res = parse(tokens);

        let strings = StringCache::new(&res.root, &mut diagnostics);
        assert_eq!(strings.ranges.len(), 7);
        assert_eq!(
            strings.ranges.get(&TextRange::new(6.into(), 11.into())),
            Some(&Range { start: 4, end: 7 })
        );
        assert_eq!(
            strings.ranges.get(&TextRange::new(35.into(), 45.into())),
            Some(&Range { start: 15, end: 22 })
        );
        assert_eq!(
            strings.ranges.get(&TextRange::new(52.into(), 75.into())),
            Some(&Range { start: 26, end: 43 })
        );
        assert_eq!(
            strings.data,
            "namefoonamenameinvalidnamevalid \t \n \\ \" foo"
        );

        insta::assert_debug_snapshot!(diagnostics);
        insta::assert_debug_snapshot!(res.reports);
    }
}
