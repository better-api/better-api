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

    fn emit(&mut self, kind: Kind) -> Token<'a> {
        let value = &self.source[self.start..self.pos];
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
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let ch = self.chars.next()?;
        self.pos += ch.len_utf8();

        let token = match ch {
            '\n' => self.emit(TOKEN_EOL),
            ch if ch.is_whitespace() => self.whitespace(),

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
}
