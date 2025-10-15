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

use better_api_diagnostic::{Label, Report, Span};
use better_api_syntax::ast::AstNode;
use better_api_syntax::{Kind, SyntaxToken, ast};
use better_api_syntax::{NodeOrToken, TextRange};

/// Cache of all the strings in the AST.
#[derive(Debug)]
pub struct StringCache(HashMap<TextRange, String>);

impl StringCache {
    pub fn new(root: &ast::Root, diagnostics: &mut Vec<Report>) -> Self {
        let mut cache = Self(HashMap::default());

        for node in root.syntax().descendants_with_tokens() {
            match node {
                NodeOrToken::Node(_) => (),
                NodeOrToken::Token(t) => {
                    if t.kind() == Kind::TOKEN_STRING {
                        cache.add_string(t, diagnostics);
                    }
                }
            }
        }

        cache
    }

    pub fn get(&self, token: &SyntaxToken) -> Option<&str> {
        if token.kind() != Kind::TOKEN_STRING {
            return None;
        }

        self.0.get(&token.text_range()).map(|s| s.as_str())
    }

    fn add_string(&mut self, token: SyntaxToken, diagnostics: &mut Vec<Report>) {
        let mut text = token.text();
        debug_assert!(&text[0..1] == "\"" && &text[text.len() - 1..text.len()] == "\"");
        text = &text[1..text.len() - 1]; // Remove start and end `"`

        let start: usize = token.text_range().start().into();

        let mut chars = text.char_indices();
        let mut res = String::new();

        while let Some((idx, ch)) = chars.next() {
            if ch != '\\' {
                res.push(ch);
                continue;
            }

            let Some((end, esc)) = chars.next() else {
                diagnostics.push(
                    Report::error("expected escaped character, got '\"'".to_string()).with_label(
                        Label::new(
                            "expected escaped character".to_string(),
                            Span::new(start + idx, start + idx + 1),
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
                    diagnostics.push(
                        Report::error(format!("got invalid escape character `{esc}`")).with_label(
                            Label::new(
                                "invalid escape character".to_string(),
                                Span::new(start + idx, start + end),
                            ),
                        ),
                    );
                    res.push(esc);
                }
            }
        }

        self.0.insert(token.text_range(), res);
    }
}

#[cfg(test)]
mod test {
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
        assert_eq!(strings.0.len(), 3);
        assert_eq!(
            strings.0.get(&TextRange::new(6.into(), 11.into())),
            Some(&"foo".to_string())
        );
        assert_eq!(
            strings.0.get(&TextRange::new(35.into(), 45.into())),
            Some(&"invalid".to_string())
        );
        assert_eq!(
            strings.0.get(&TextRange::new(52.into(), 75.into())),
            Some(&"valid \t \n \\ \" foo".to_string())
        );

        insta::assert_debug_snapshot!(diagnostics);
        insta::assert_debug_snapshot!(res.reports);
    }
}
