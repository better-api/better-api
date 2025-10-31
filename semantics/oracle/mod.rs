//! Defines core type responsible for semantic analysis.

use better_api_syntax::ast;

use crate::{typ, value};

/// Core type responsible for semantic analysis.
#[derive(Clone, Default)]
pub struct Oracle {
    strings: string_interner::DefaultStringInterner,
    values: value::ValueArena,
    types: typ::TypeArena,
}

impl Oracle {
    /// Create a new oracle.
    ///
    /// Runs semantic analysis on the given AST and creates an oracle
    /// that can be queried for semantics info.
    pub fn new(root: &ast::Root) -> Self {
        let mut oracle = Self::default();
        oracle.analyze(root);
        oracle
    }

    /// Runs semantic analysis on the given AST and re-creates the oracle.
    ///
    /// This method replaces the information stored in the oracle by
    /// reusing the allocated memory. A good place to use this method is in
    /// an LSP where semantic analysis runs on every file change. Instead of
    /// dropping an oracle and creating a new one, this method should be used
    /// to avoid unnecessary heap re-allocations.
    pub fn reanalyze(&mut self, root: &ast::Root) {
        self.strings = Default::default();
        self.values.clear();
        self.types.clear();

        self.analyze(root);
    }

    fn analyze(&mut self, _root: &ast::Root) {
        // TODO: Implement the actual analysis
    }
}
