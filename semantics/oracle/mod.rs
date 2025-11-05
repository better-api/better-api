//! Defines core type responsible for semantic analysis.

use better_api_diagnostic::Report;
use better_api_syntax::ast;

use crate::SourceMap;
use crate::typ::TypeArena;
use crate::value::ValueArena;

mod metadata;
mod value;

/// Core type responsible for semantic analysis.
#[derive(Clone, Default)]
pub struct Oracle {
    // Containers for primary oracle data
    strings: string_interner::DefaultStringInterner,
    values: ValueArena,
    types: TypeArena,

    source_map: SourceMap,

    // Reports generated during semantic analysis
    reports: Vec<Report>,
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

    /// Get semantic problems.
    pub fn reports(&self) -> &[Report] {
        &self.reports
    }

    fn analyze(&mut self, root: &ast::Root) {
        self.analyze_metadata(root);
        // TODO: Implement the actual analysis
    }
}
