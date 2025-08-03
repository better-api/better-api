mod tokenizer;

pub use tokenizer::{Token, tokenize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(non_camel_case_types)]
#[repr(u16)]
pub enum Kind {
    TOKEN_SPACE = 0, // white space without new line (' ', '\t', ...)
    TOKEN_EOL,       // End of line (new line)
    TOKEN_ERROR,

    TOKEN_IDENTIFIER,
    TOKEN_STRING,
    TOKEN_INTEGER,
    TOKEN_FLOAT,

    TOKEN_COMMENT,     // '//  comment'
    TOKEN_DOC_COMMENT, // '/// doc comment'
    TOKEN_TOP_COMMENT, // '//! top comment'

    TOKEN_COMMA,         // ,
    TOKEN_COLON,         // :
    TOKEN_OPTION,        // ?
    TOKEN_BRACKET_LEFT,  // [
    TOKEN_BRACKET_RIGHT, // ]
    TOKEN_PAREN_LEFT,    // (
    TOKEN_PAREN_RIGHT,   // )
    TOKEN_CURLY_LEFT,    // {
    TOKEN_CURLY_RIGHT,   // }

    TOKEN_KW_GET,
    TOKEN_KW_POST,
    TOKEN_KW_PUT,
    TOKEN_KW_DELETE,
    TOKEN_KW_PATCH,

    TOKEN_KW_TRUE,      // true
    TOKEN_KW_FALSE,     // false
    TOKEN_KW_DEFAULT,   // @default
    TOKEN_KW_I32,       // i32
    TOKEN_KW_I64,       // i64
    TOKEN_KW_U32,       // u32
    TOKEN_KW_U64,       // u64
    TOKEN_KW_F32,       // f32
    TOKEN_KW_F64,       // f64
    TOKEN_KW_DATE,      // date
    TOKEN_KW_TIMESTAMP, // timestamp
    TOKEN_KW_BOOL,      // bool
    TOKEN_KW_STRING,    // string
    TOKEN_KW_FILE,      // file
}

