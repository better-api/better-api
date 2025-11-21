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
    TypeDefinition(StringId),
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

    /// Get [`Element`] for provided syntax node
    fn get_fwd(&self, node: &impl AstNode<Language = Language>) -> Option<&Element> {
        let ptr = SyntaxNodePtr::new(node.syntax());

        self.fwd.get(&ptr)
    }

    /// Get syntax node for provided [`Element`].
    ///
    /// **Note:** If source map is constructed by correctly working [`Oracle`],
    /// every [`Element`] should have a syntax node. This method will panic if given
    /// [`Element`] doesn't have a syntax node. In which case there is something wrong
    /// with [`Oracle`] implementation.
    fn get_bck(&self, element: &Element) -> SyntaxNodePtr {
        *self
            .bck
            .get(element)
            .expect("oracle should construct source map correctly")
    }
}
