//! Defines core type responsible for semantic analysis.

use better_api_diagnostic::Report;
use better_api_syntax::ast;

use crate::SourceMap;
use crate::typ::TypeArena;
use crate::value::ValueArena;

mod metadata;
mod typ;
mod value;

/// Core type responsible for semantic analysis.
#[derive(Clone)]
pub struct Oracle<'a> {
    /// Root node of the parsed tree.
    root: &'a ast::Root,

    // Containers for primary oracle data
    strings: string_interner::DefaultStringInterner,
    values: ValueArena,
    types: TypeArena,

    source_map: SourceMap,

    // Reports generated during semantic analysis
    reports: Vec<Report>,
}

impl<'a> Oracle<'a> {
    /// Create a new oracle.
    ///
    /// Runs semantic analysis on the given AST and creates an oracle
    /// that can be queried for semantics info.
    pub fn new(root: &'a ast::Root) -> Self {
        let mut oracle = Self {
            root,
            strings: Default::default(),
            values: Default::default(),
            types: Default::default(),
            source_map: Default::default(),
            reports: Default::default(),
        };

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
