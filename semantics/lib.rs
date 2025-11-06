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
    EnumMember(value::ValueId),
}

/// Maps syntax nodes to semantic elements.
#[derive(Default, Clone)]
struct SourceMap {
    fwd: HashMap<SyntaxNodePtr, Element>,
    bck: HashMap<Element, SyntaxNodePtr>,
}

impl SourceMap {
    /// Insert source map info
    fn insert(&mut self, node: &impl AstNode<Language = Language>, element: Element) {
        let ptr = SyntaxNodePtr::new(node.syntax());

        self.fwd.insert(ptr, element);
        self.bck.insert(element, ptr);
    }
}
