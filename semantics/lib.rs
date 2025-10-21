use better_api_syntax::{SyntaxNode, SyntaxNodePtr};

pub mod name;
pub mod text;
pub mod typ;
pub mod value;

/// Semantic element associated with a syntax node pointer.
#[derive(Debug, Clone, PartialEq)]
pub struct Tracked<T> {
    pub syntax: SyntaxNodePtr,
    pub data: T,
}

impl<T> Tracked<T> {
    pub fn new(node: &SyntaxNode, data: T) -> Self {
        Self {
            syntax: SyntaxNodePtr::new(node),
            data,
        }
    }
}
