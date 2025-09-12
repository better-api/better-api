use crate::Kind::{self, *};
use better_api_diagnostic::{Label, Report, Span};

/// A single token. It's a pair of token type and the underlying string.
pub type Token<'a> = (Kind, &'a str);

/// Tokenizes a string and return an iterator of tokens.
///
/// Any diagnostics that should be reported are appended into the
/// `diagnostics` vector provided by the caller.
pub fn tokenize<'s>(
    source: &'s str,
    diagnostics: &mut Vec<Report>,
) -> impl Iterator<Item = Token<'s>> {
    Tokenizer::new(source, diagnostics)
}

struct Tokenizer<'s, 'd> {
    source: &'s str,
    chars: std::iter::Peekable<std::str::Chars<'s>>,

    /// Start position of the token being parsed.
    start: usize,
    /// Current position of the tokenizer.
    pos: usize,

    reports: &'d mut Vec<Report>,
}

impl<'s, 'd> Tokenizer<'s, 'd> {
    fn new(source: &'s str, diagnostics: &'d mut Vec<Report>) -> Self {
        Tokenizer {
            source,
            chars: source.chars().peekable(),
            start: 0,
            pos: 0,
            reports: diagnostics,
        }
    }

    /// Returns the string value of the token that is currently being parsed.
    fn current_value(&self) -> &'s str {
        &self.source[self.start..self.pos]
    }

    /// Emits the currently parsed token and resets starting positions for
    /// next token.
    fn emit(&mut self, kind: Kind) -> Token<'s> {
        let value = self.current_value();
        self.start = self.pos;
        (kind, value)
    }

    /// Tokenizes white space that is not EOL '\n'.
    fn whitespace(&mut self) -> Token<'s> {
        while let Some(ch) = self.chars.peek()
            && ch.is_whitespace()
            && *ch != '\n'
        {
            // Move to next char
            self.pos += ch.len_utf8();
            self.chars.next();
        }

        self.emit(TOKEN_SPACE)
    }

    /// Reds identifier, but doesn't emit a token. This is because the function
    /// is also used for reading keywords.
    fn read_ident(&mut self) {
        while let Some(ch) = self.chars.peek()
            && (ch.is_alphabetic() || ch.is_ascii_digit() || *ch == '_' || *ch == '-')
        {
            // Move to next char
            self.pos += ch.len_utf8();
            self.chars.next();
        }
    }

    /// Tokenizes a string
    fn string(&mut self) -> Token<'s> {
        while let Some(ch) = self.chars.peek()
            && *ch != '"'
        {
            if *ch == '\n' {
                self.reports.push(
                    Report::error("missing string ending quotes '\"'".to_string()).with_label(
                        Label::new(
                            "expected '\"' before new line".to_string(),
                            Span::new(self.pos, self.pos + ch.len_utf8()),
                        ),
                    ),
                );

                return self.emit(TOKEN_ERROR);
            }

            // Move to next char
            self.pos += ch.len_utf8();
            let ch = self.chars.next().unwrap();

            // If we have escape sequence, we "ignore" the next character.
            // The validity of escape sequences is checked during tree parsing.
            // This was we can have ERROR_NODE and STRING_TOKEN as child, which
            // preserves more information than ERROR_TOKEN.
            if ch == '\\' {
                let ch = self.chars.next();
                self.pos += ch.map_or(0, |c| c.len_utf8());
            }
        }

        let ch = self.chars.next();
        self.pos += ch.map_or(0, |c| c.len_utf8());

        match ch {
            None => {
                self.reports.push(
                    Report::error("missing string ending quotes '\"'".to_string()).with_label(
                        Label::new(
                            "expected '\"' before EOF".to_string(),
                            // We assume that the last character is one byte width. If this is not true
                            // we might have a strange report, but for now this is fine.
                            Span::new(self.pos - 1, self.pos),
                        ),
                    ),
                );
                self.emit(TOKEN_ERROR)
            }
            Some('"') => self.emit(TOKEN_STRING),

            // Unreachable because while loop runs until '"'
            Some(_) => unreachable!(),
        }
    }

    /// Tokenizes a number (int or float)
    fn number(&mut self) -> Token<'s> {
        let mut nr_dots = 0;

        while let Some(ch) = self.chars.peek()
            && (ch.is_ascii_digit() || *ch == '.')
        {
            if *ch == '.' {
                nr_dots += 1;
            }

            self.pos += ch.len_utf8();
            self.chars.next();
        }

        // Check if the number ended with '.'. In this case it's invalid float.
        // Since digits and '.' are one byte size, the following sub-string is valid.
        let last_ch = &self.source[self.pos - 1..self.pos];
        if last_ch == "." {
            self.reports.push(
                Report::error(format!("invalid float `{}`", self.current_value()))
                    .with_label(Label::new(
                        "invalid float".to_string(),
                        Span::new(self.start, self.pos),
                    ))
                    .with_note(
                        "help: float can't end with '.'. You should end it with '.0' instead."
                            .to_string(),
                    ),
            );
            return self.emit(TOKEN_ERROR);
        }

        match nr_dots {
            0 => self.emit(TOKEN_INTEGER),
            1 => self.emit(TOKEN_FLOAT),
            _ => {
                self.reports.push(
                    Report::error(format!("invalid float `{}`", self.current_value())).with_label(
                        Label::new("invalid float".to_string(), Span::new(self.start, self.pos)),
                    ),
                );

                self.emit(TOKEN_ERROR)
            }
        }
    }

    /// Tokenizes a comment
    fn comment(&mut self) -> Token<'s> {
        let ch = self.chars.next().unwrap();
        self.pos += ch.len_utf8();

        let token_type = match self.chars.peek() {
            Some('/') => {
                let ch = self.chars.next().unwrap();
                self.pos += ch.len_utf8();
                TOKEN_DOC_COMMENT
            }
            Some('!') => {
                let ch = self.chars.next().unwrap();
                self.pos += ch.len_utf8();
                TOKEN_TOP_COMMENT
            }
            _ => TOKEN_COMMENT,
        };

        while let Some(ch) = self.chars.peek()
            && *ch != '\n'
        {
            self.pos += ch.len_utf8();
            self.chars.next();
        }

        self.emit(token_type)
    }

    fn keyword(&mut self) -> Token<'s> {
        self.read_ident();
        let value = self.current_value();
        let kind = match value {
            "GET" => TOKEN_KW_GET,
            "POST" => TOKEN_KW_POST,
            "PUT" => TOKEN_KW_PUT,
            "DELETE" => TOKEN_KW_DELETE,
            "PATCH" => TOKEN_KW_PATCH,

            "true" => TOKEN_KW_TRUE,
            "false" => TOKEN_KW_FALSE,

            "i32" => TOKEN_KW_I32,
            "i64" => TOKEN_KW_I64,
            "u32" => TOKEN_KW_U32,
            "u64" => TOKEN_KW_U64,
            "f32" => TOKEN_KW_F32,
            "f64" => TOKEN_KW_F64,
            "date" => TOKEN_KW_DATE,
            "timestamp" => TOKEN_KW_TIMESTAMP,
            "bool" => TOKEN_KW_BOOL,
            "string" => TOKEN_KW_STRING,
            "file" => TOKEN_KW_FILE,

            "type" => TOKEN_KW_TYPE,
            "rec" => TOKEN_KW_REC,
            "enum" => TOKEN_KW_ENUM,
            "union" => TOKEN_KW_UNION,
            "resp" => TOKEN_KW_RESP,

            _ => TOKEN_IDENTIFIER,
        };

        self.emit(kind)
    }
}

impl<'s, 'd> Iterator for Tokenizer<'s, 'd> {
    type Item = Token<'s>;

    fn next(&mut self) -> Option<Self::Item> {
        let ch = self.chars.next()?;
        self.pos += ch.len_utf8();

        let token = match ch {
            '\n' => self.emit(TOKEN_EOL),
            ch if ch.is_whitespace() && ch != '\n' => self.whitespace(),

            '"' => self.string(),

            ',' => self.emit(TOKEN_COMMA),
            ':' => self.emit(TOKEN_COLON),
            '?' => self.emit(TOKEN_OPTION),
            '(' => self.emit(TOKEN_PAREN_LEFT),
            ')' => self.emit(TOKEN_PAREN_RIGHT),
            '[' => self.emit(TOKEN_BRACKET_LEFT),
            ']' => self.emit(TOKEN_BRACKET_RIGHT),
            '{' => self.emit(TOKEN_CURLY_LEFT),
            '}' => self.emit(TOKEN_CURLY_RIGHT),

            '@' => {
                self.read_ident();
                let value = self.current_value();
                match value {
                    "@default" => self.emit(TOKEN_KW_DEFAULT),

                    _ => {
                        self.reports.push(
                            Report::error(format!("invalid decorator `{value}`")).with_label(
                                Label::new(
                                    "invalid decorator".to_string(),
                                    Span::new(self.start, self.pos),
                                ),
                            ),
                        );
                        self.emit(TOKEN_ERROR)
                    }
                }
            }

            ch if ch.is_ascii_digit() => self.number(),

            ch if ch.is_alphabetic() => self.keyword(),

            '/' => match self.chars.peek() {
                Some('/') => self.comment(),
                _ => self.emit(TOKEN_ERROR),
            },

            _ => {
                self.reports.push(
                    Report::error(format!("unknown token `{}`", self.current_value())).with_label(
                        Label::new("unknown token".to_string(), Span::new(self.start, self.pos)),
                    ),
                );
                self.emit(TOKEN_ERROR)
            }
        };

        Some(token)
    }
}

#[cfg(test)]
mod test {
    use better_api_diagnostic::{Label, Report, Span};
    use indoc::indoc;

    use super::tokenize;
    use crate::Kind::*;

    #[test]
    fn simple_whitespace() {
        let mut diagnostics = vec![];
        let tokens: Vec<_> = tokenize(" \t \n    \t ", &mut diagnostics).collect();

        assert_eq!(
            tokens,
            vec![
                (TOKEN_SPACE, " \t "),
                (TOKEN_EOL, "\n"),
                (TOKEN_SPACE, "    \t ")
            ]
        );
        assert_eq!(diagnostics, vec![]);
    }

    #[test]
    fn simple_keywords_and_idents() {
        let mut diagnostics = vec![];
        let tokens: Vec<_> = tokenize(
            indoc! {r#"
                GET
                POST
                PUT
                DELETE
                PATCH
                true
                false
                this_is_Ident213-890asdf
                i32
                i64
                f32
                f64
                u32
                u64
                date
                timestamp
                bool
                string
                file
            "#},
            &mut diagnostics,
        )
        .collect();

        assert_eq!(
            tokens,
            vec![
                (TOKEN_KW_GET, "GET"),
                (TOKEN_EOL, "\n"),
                (TOKEN_KW_POST, "POST"),
                (TOKEN_EOL, "\n"),
                (TOKEN_KW_PUT, "PUT"),
                (TOKEN_EOL, "\n"),
                (TOKEN_KW_DELETE, "DELETE"),
                (TOKEN_EOL, "\n"),
                (TOKEN_KW_PATCH, "PATCH"),
                (TOKEN_EOL, "\n"),
                (TOKEN_KW_TRUE, "true"),
                (TOKEN_EOL, "\n"),
                (TOKEN_KW_FALSE, "false"),
                (TOKEN_EOL, "\n"),
                (TOKEN_IDENTIFIER, "this_is_Ident213-890asdf"),
                (TOKEN_EOL, "\n"),
                (TOKEN_KW_I32, "i32"),
                (TOKEN_EOL, "\n"),
                (TOKEN_KW_I64, "i64"),
                (TOKEN_EOL, "\n"),
                (TOKEN_KW_F32, "f32"),
                (TOKEN_EOL, "\n"),
                (TOKEN_KW_F64, "f64"),
                (TOKEN_EOL, "\n"),
                (TOKEN_KW_U32, "u32"),
                (TOKEN_EOL, "\n"),
                (TOKEN_KW_U64, "u64"),
                (TOKEN_EOL, "\n"),
                (TOKEN_KW_DATE, "date"),
                (TOKEN_EOL, "\n"),
                (TOKEN_KW_TIMESTAMP, "timestamp"),
                (TOKEN_EOL, "\n"),
                (TOKEN_KW_BOOL, "bool"),
                (TOKEN_EOL, "\n"),
                (TOKEN_KW_STRING, "string"),
                (TOKEN_EOL, "\n"),
                (TOKEN_KW_FILE, "file"),
                (TOKEN_EOL, "\n"),
            ]
        );
        assert_eq!(diagnostics, vec![]);
    }

    #[test]
    fn modifier_keywords() {
        let mut diagnostics = vec![];
        let tokens: Vec<_> = tokenize("@default @error", &mut diagnostics).collect();

        assert_eq!(
            tokens,
            vec![
                (TOKEN_KW_DEFAULT, "@default"),
                (TOKEN_SPACE, " "),
                (TOKEN_ERROR, "@error"),
            ]
        );
        assert_eq!(
            diagnostics,
            vec![
                Report::error("invalid decorator `@error`".to_string()).with_label(Label::new(
                    "invalid decorator".to_string(),
                    Span::new(9, 15)
                ))
            ]
        );
    }

    #[test]
    fn string_simple() {
        let mut diagnostics = vec![];
        let tokens: Vec<_> =
            tokenize(r#""foo""bar""something longer 1@*-""#, &mut diagnostics).collect();

        assert_eq!(
            tokens,
            vec![
                (TOKEN_STRING, "\"foo\""),
                (TOKEN_STRING, "\"bar\""),
                (TOKEN_STRING, "\"something longer 1@*-\""),
            ]
        );
        assert_eq!(diagnostics, vec![]);
    }

    #[test]
    fn unfinished_string() {
        let mut diagnostics = vec![];
        let tokens: Vec<_> = tokenize("\"foo\n\"foo", &mut diagnostics).collect();

        assert_eq!(
            tokens,
            vec![
                (TOKEN_ERROR, "\"foo"),
                (TOKEN_EOL, "\n"),
                (TOKEN_ERROR, "\"foo"),
            ]
        );
        assert_eq!(
            diagnostics,
            vec![
                Report::error("missing string ending quotes '\"'".to_string()).with_label(
                    Label::new("expected '\"' before new line".to_string(), Span::new(4, 5))
                ),
                Report::error("missing string ending quotes '\"'".to_string()).with_label(
                    Label::new("expected '\"' before EOF".to_string(), Span::new(8, 9))
                ),
            ]
        );
    }

    #[test]
    fn string_escape() {
        let mut diagnostics = vec![];
        let tokens: Vec<_> = tokenize(r#" "foo \" \a \b \c \ " "#, &mut diagnostics).collect();
        assert_eq!(
            tokens,
            vec![
                (TOKEN_SPACE, " "),
                (TOKEN_STRING, r#""foo \" \a \b \c \ ""#),
                (TOKEN_SPACE, " "),
            ]
        );
        assert_eq!(diagnostics, vec![]);
    }

    #[test]
    fn number() {
        let mut diagnostics = vec![];
        let tokens: Vec<_> = tokenize("10 42.69 1.2.3 1.", &mut diagnostics).collect();

        assert_eq!(
            tokens,
            vec![
                (TOKEN_INTEGER, "10"),
                (TOKEN_SPACE, " "),
                (TOKEN_FLOAT, "42.69"),
                (TOKEN_SPACE, " "),
                (TOKEN_ERROR, "1.2.3"),
                (TOKEN_SPACE, " "),
                (TOKEN_ERROR, "1.")
            ]
        );
        assert_eq!(
            diagnostics,
            vec![
                Report::error("invalid float `1.2.3`".to_string())
                    .with_label(Label::new("invalid float".to_string(), Span::new(9, 14))),
                Report::error("invalid float `1.`".to_string())
                    .with_label(Label::new("invalid float".to_string(), Span::new(15, 17)))
                    .with_note(
                        "help: float can't end with '.'. You should end it with '.0' instead."
                            .to_string()
                    )
            ]
        );
    }

    #[test]
    fn comments() {
        let mut diagnostics = vec![];
        let tokens: Vec<_> = tokenize(
            indoc! {r#"
                //! Top comment
                /// doc comment
                //comment
            "#},
            &mut diagnostics,
        )
        .collect();

        assert_eq!(
            tokens,
            vec![
                (TOKEN_TOP_COMMENT, "//! Top comment"),
                (TOKEN_EOL, "\n"),
                (TOKEN_DOC_COMMENT, "/// doc comment"),
                (TOKEN_EOL, "\n"),
                (TOKEN_COMMENT, "//comment"),
                (TOKEN_EOL, "\n"),
            ]
        );
        assert_eq!(diagnostics, vec![]);
    }

    #[test]
    fn types() {
        let mut diagnostics = vec![];
        let tokens: Vec<_> = tokenize(
            indoc! {r#"
                type Foo: rec {}
                type Foo: enum(string) {}
                type Foo: union("type") {}
                type Foo: resp {}
            "#},
            &mut diagnostics,
        )
        .collect();

        insta::assert_debug_snapshot!(tokens);
        assert_eq!(diagnostics, vec![]);
    }
}
