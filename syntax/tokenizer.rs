use crate::Kind::{self, *};

pub type Token<'a> = (Kind, &'a str);

pub fn tokenize<'a>(source: &'a str) -> impl Iterator<Item = Token<'a>> {
    Tokenizer::new(source)
}

struct Tokenizer<'a> {
    source: &'a str,
    chars: std::iter::Peekable<std::str::Chars<'a>>,

    /// Start position of the token being parsed.
    start: usize,
    /// Current position of the tokenizer.
    pos: usize,
}

impl<'a> Tokenizer<'a> {
    fn new(source: &'a str) -> Self {
        Tokenizer {
            source,
            chars: source.chars().peekable(),
            start: 0,
            pos: 0,
        }
    }

    fn current_value(&self) -> &'a str {
        &self.source[self.start..self.pos]
    }

    fn emit(&mut self, kind: Kind) -> Token<'a> {
        let value = self.current_value();
        self.start = self.pos;
        (kind, value)
    }

    fn whitespace(&mut self) -> Token<'a> {
        while let Some(ch) = self.chars.peek()
            && ch.is_whitespace()
            && *ch != '\n'
        {
            self.pos += ch.len_utf8();

            // Move to next char
            self.chars.next();
        }

        self.emit(TOKEN_SPACE)
    }

    fn read_ident(&mut self) {
        while let Some(ch) = self.chars.peek()
            && (ch.is_alphabetic() || ch.is_ascii_digit() || *ch == '_' || *ch == '-')
        {
            self.pos += ch.len_utf8();

            // Move to next char
            self.chars.next();
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let ch = self.chars.next()?;
        self.pos += ch.len_utf8();

        let token = match ch {
            '\n' => self.emit(TOKEN_EOL),
            ch if ch.is_whitespace() && ch != '\n' => self.whitespace(),

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

                    // TODO: Emit diagnostics
                    _ => self.emit(TOKEN_ERROR),
                }
            }

            ch if ch.is_alphabetic() => {
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

                    _ => TOKEN_IDENTIFIER,
                };

                self.emit(kind)
            }

            // TODO: Emit diagnostics
            _ => self.emit(TOKEN_ERROR),
        };

        Some(token)
    }
}

#[cfg(test)]
mod test {
    use super::tokenize;
    use crate::Kind::*;

    #[test]
    fn simple_whitespace() {
        let tokens: Vec<_> = tokenize(" \t \n    \t ").collect();
        assert_eq!(
            tokens,
            vec![
                (TOKEN_SPACE, " \t "),
                (TOKEN_EOL, "\n"),
                (TOKEN_SPACE, "    \t ")
            ]
        );
    }

    #[test]
    fn simple_keywords_and_idents() {
        let tokens: Vec<_> = tokenize(
            r#"GET
POST
PUT
DELETE
PATCH
true
false
this_is_Ident213-890asdf"#,
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
            ]
        );
    }

    #[test]
    fn modifier_keywords() {
        let tokens: Vec<_> = tokenize("@default @error").collect();

        assert_eq!(
            tokens,
            vec![
                (TOKEN_KW_DEFAULT, "@default"),
                (TOKEN_SPACE, " "),
                (TOKEN_ERROR, "@error"),
            ]
        );
    }
}
