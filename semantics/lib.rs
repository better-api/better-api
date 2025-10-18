use better_api_syntax::SyntaxNodePtr;

pub mod name;
pub mod text;
pub mod typ;
pub mod value;

pub struct Tracked<T> {
    pub syntax: SyntaxNodePtr,
    pub data: T,
}
