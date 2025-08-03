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

    /// Returns the string value of the token that is currently being parsed.
    fn current_value(&self) -> &'a str {
        &self.source[self.start..self.pos]
    }

    /// Emits the currently parsed token and resets starting positions for
    /// next token.
    fn emit(&mut self, kind: Kind) -> Token<'a> {
        let value = self.current_value();
        self.start = self.pos;
        (kind, value)
    }

    /// Tokenizes white space that is not EOL '\n'.
    fn whitespace(&mut self) -> Token<'a> {
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
    fn string(&mut self) -> Token<'a> {
        while let Some(ch) = self.chars.peek()
            && *ch != '"'
        {
            if *ch == '\n' {
                // TODO: Emit diagnostics
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
            // TODO: Emit diagnostics
            None => self.emit(TOKEN_ERROR),
            Some('"') => self.emit(TOKEN_STRING),
            // Unreachable because while loop runs until '"'
            Some(_) => unreachable!(),
        }
    }

    /// Tokenizes a number (int or float)
    fn number(&mut self) -> Token<'a> {
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
            return self.emit(TOKEN_ERROR);
        }

        match nr_dots {
            0 => self.emit(TOKEN_INTEGER),
            1 => self.emit(TOKEN_FLOAT),
            // TODO: Emit diagnostics
            _ => self.emit(TOKEN_ERROR),
        }
    }

    /// Tokenizes a comment
    fn comment(&mut self) -> Token<'a> {
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

    fn keyword(&mut self) -> Token<'a> {
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

            _ => TOKEN_IDENTIFIER,
        };

        self.emit(kind)
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

                    // TODO: Emit diagnostics
                    _ => self.emit(TOKEN_ERROR),
                }
            }

            ch if ch.is_ascii_digit() => self.number(),

            ch if ch.is_alphabetic() => self.keyword(),

            '/' => match self.chars.peek() {
                Some('/') => self.comment(),
                _ => self.emit(TOKEN_ERROR),
            },

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
file"#,
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

    #[test]
    fn string_simple() {
        let tokens: Vec<_> = tokenize(r#""foo""bar""something longer 1@*-""#).collect();

        assert_eq!(
            tokens,
            vec![
                (TOKEN_STRING, "\"foo\""),
                (TOKEN_STRING, "\"bar\""),
                (TOKEN_STRING, "\"something longer 1@*-\""),
            ]
        );
    }

    #[test]
    fn unfinished_string() {
        let tokens: Vec<_> = tokenize("\"foo\n\"foo").collect();

        assert_eq!(
            tokens,
            vec![
                (TOKEN_ERROR, "\"foo"),
                (TOKEN_EOL, "\n"),
                (TOKEN_ERROR, "\"foo"),
            ]
        );
    }

    #[test]
    fn string_escape() {
        let tokens: Vec<_> = tokenize(r#" "foo \" \a \b \c \ " "#).collect();
        assert_eq!(
            tokens,
            vec![
                (TOKEN_SPACE, " "),
                (TOKEN_STRING, r#""foo \" \a \b \c \ ""#),
                (TOKEN_SPACE, " "),
            ]
        );
    }

    #[test]
    fn number() {
        let tokens: Vec<_> = tokenize("10 42.69 1.2.3 1.").collect();

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
        )
    }

    #[test]
    fn comments() {
        let tokens: Vec<_> = tokenize(
            r#"//! Top comment
/// doc comment
//comment"#,
        )
        .collect();

        assert_eq!(
            tokens,
            vec![
                (TOKEN_TOP_COMMENT, "//! Top comment"),
                (TOKEN_EOL, "\n"),
                (TOKEN_DOC_COMMENT, "/// doc comment"),
                (TOKEN_EOL, "\n"),
                (TOKEN_COMMENT, "//comment")
            ]
        );
    }
}
