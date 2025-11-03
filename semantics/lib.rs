mod text;

pub mod oracle;
pub mod typ;
pub mod value;

use std::collections::HashMap;

use better_api_syntax::{Language, SyntaxNodePtr, ast::AstNode};
/// Re-export for simpler use case.
pub use oracle::Oracle;

/// Id of an interned string.
pub type StringId = string_interner::DefaultSymbol;

/// Semantic element.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Element {
    Value(value::ValueId),
    ObjectField(value::ObjectFieldId),
    Type(typ::TypeId),
    TypeField(typ::TypeFieldId),
}

impl From<value::ValueId> for Element {
    fn from(value: value::ValueId) -> Self {
        Self::Value(value)
    }
}

impl From<value::ObjectFieldId> for Element {
    fn from(value: value::ObjectFieldId) -> Self {
        Self::ObjectField(value)
    }
}

impl From<typ::TypeId> for Element {
    fn from(value: typ::TypeId) -> Self {
        Self::Type(value)
    }
}

impl From<typ::TypeFieldId> for Element {
    fn from(value: typ::TypeFieldId) -> Self {
        Self::TypeField(value)
    }
}

/// Maps syntax nodes to semantic elements.
#[derive(Default, Clone)]
struct SourceMap {
    fwd: HashMap<SyntaxNodePtr, Element>,
    bck: HashMap<Element, SyntaxNodePtr>,
}

impl SourceMap {
    fn insert(&mut self, node: &impl AstNode<Language = Language>, element: Element) {
        let ptr = SyntaxNodePtr::new(node.syntax());

        self.fwd.insert(ptr, element);
        self.bck.insert(element, ptr);
    }
}
