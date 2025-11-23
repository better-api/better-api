//! Helper functions for parsing string tokens and validating names.

use std::borrow::Cow;

use better_api_diagnostic::{Label, Report, Span};
use better_api_syntax::{Kind, SyntaxToken, TextRange, ast};
use string_interner::DefaultStringInterner;

use crate::StringId;

/// Lowers name by parsing, validating and interning it.
///
/// Returns interned string id of the name if it's valid.
pub fn lower_name(
    name: &ast::Name,
    strings: &mut DefaultStringInterner,
    reports: &mut Vec<Report>,
) -> Option<StringId> {
    let token = name.token();

    let name_str: Cow<_> = match &token {
        ast::NameToken::Identifier(ident) => ident.text().into(),
        ast::NameToken::String(string) => parse_string(string, reports),
    };

    if let Err(report) = validate_name(&name_str, token.text_range()) {
        reports.push(report);
        return None;
    }

    let name_id = strings.get_or_intern(name_str);
    Some(name_id)
}

/// Validates if given string is a valid name.
pub fn validate_name(name: &str, range: TextRange) -> Result<(), Report> {
    let is_valid = name
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-' || c == '.')
        && name.chars().next().is_some_and(|c| c.is_ascii_alphabetic());

    if is_valid {
        Ok(())
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
/// <div class="warning">
/// This function expect a string token. Any other token results in a unknown behavior.
/// </div>
pub fn parse_string<'a>(token: &'a SyntaxToken, diagnostics: &mut Vec<Report>) -> Cow<'a, str> {
    debug_assert_eq!(token.kind(), Kind::TOKEN_STRING);

    let mut text = token.text();

    debug_assert!(&text[0..1] == "\"" && &text[text.len() - 1..text.len()] == "\"");
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
            diagnostics.push(
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
                diagnostics.push(
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

#[cfg(test)]
mod test {
    use better_api_diagnostic::{Label, Report, Span};
    use better_api_syntax::ast::AstNode;
    use better_api_syntax::{Kind, Parse, SyntaxToken, TextRange, TextSize, parse, tokenize};

    use crate::text::{parse_string, validate_name};

    #[test]
    fn valid_name() {
        assert_eq!(
            validate_name(
                "this-is-a.valid_name123",
                TextRange::new(TextSize::new(0), TextSize::new(1))
            ),
            Ok(())
        );
    }

    #[test]
    fn invalid_name() {
        let expected = Err(
                Report::error("invalid name".to_string())
                .add_label(Label::primary("invalid name".to_string(), Span::new(0, 1)))
                    .with_note("help: name can only contain alphanumeric characters, `_`, `-` and `.`. It also has to start with alphabetic character.".to_string()));

        assert_eq!(
            validate_name(
                "invalid name",
                TextRange::new(TextSize::new(0), TextSize::new(1))
            ),
            expected
        );
    }

    // Auxiliary function that gets first string token in tree.
    fn get_string_token(res: &Parse) -> SyntaxToken {
        res.root
            .syntax()
            .descendants_with_tokens()
            .find_map(|el| {
                el.as_token().and_then(|t| {
                    if t.kind() == Kind::TOKEN_STRING {
                        Some(t.clone())
                    } else {
                        None
                    }
                })
            })
            .unwrap()
    }

    #[test]
    fn valid_string() {
        let mut diags = vec![];
        let tokens = tokenize("name: \"foo\"", &mut diags);
        let res = parse(tokens);
        let token = get_string_token(&res);

        let res = parse_string(&token, &mut diags);
        assert_eq!(diags, vec![]);
        assert_eq!(res, "foo");
    }

    #[test]
    fn invalid_string() {
        let mut diags = vec![];
        let tokens = tokenize("name: \"inval\\id\"", &mut diags);
        let res = parse(tokens);
        let token = get_string_token(&res);

        let res = parse_string(&token, &mut diags);
        assert_eq!(
            diags,
            vec![
                Report::error("got invalid escape character `i`".to_string()).add_label(
                    Label::primary("invalid escape character".to_string(), Span::new(12, 14))
                )
            ]
        );
        assert_eq!(res, "invalid");
    }

    #[test]
    fn valid_string_with_escapes() {
        let mut diags = vec![];
        let tokens = tokenize(r#"name: "valid \t \n \\ \" foo""#, &mut diags);
        let res = parse(tokens);
        let token = get_string_token(&res);

        let res = parse_string(&token, &mut diags);
        assert_eq!(diags, vec![]);
        assert_eq!(res, "valid \t \n \\ \" foo");
    }
}
