//! Defines core type responsible for semantic analysis.

use better_api_diagnostic::Report;
use better_api_syntax::ast;

use crate::typ::TypeArena;
use crate::value::ValueArena;
use crate::{SourceMap, StringId};

mod metadata;
mod value;

/// Represents object or type field with interned name.
#[derive(Clone)]
struct InternedField<T> {
    name: StringId,
    field: T,
}

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

    // Reusable buffers used during analyzing values and objects.
    // This way we avoid unnecessary allocation.
    object_fields_buf: Vec<InternedField<ast::ObjectField>>,
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
