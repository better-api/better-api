//! Defines core type responsible for semantic analysis.

use std::collections::HashMap;

use better_api_diagnostic::Report;
use better_api_syntax::ast;

use crate::source_map::SourceMap;
use crate::string::{StringId, StringInterner};
use crate::typ::{TypeArena, TypeId};
use crate::value::ValueArena;

mod metadata;
mod symbols;
mod typ;
mod value;

#[cfg(test)]
mod tests;

/// Core type responsible for semantic analysis.
#[derive(Clone)]
pub struct Oracle<'a> {
    // Containers for primary oracle data
    strings: StringInterner,
    values: ValueArena,
    types: TypeArena,
    symbol_table: HashMap<StringId, TypeId>,

    source_map: SourceMap<'a>,

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
            strings: Default::default(),
            values: Default::default(),
            types: Default::default(),
            symbol_table: Default::default(),
            source_map: SourceMap::new(root),
            reports: Default::default(),
        };

        oracle.analyze(root);
        oracle
    }

    /// Get semantic problems.
    pub fn reports(&self) -> &[Report] {
        &self.reports
    }

    /// Analyzes the complete syntax tree and populates the analyzer arenas.
    ///
    /// The analysis is performed in strictly sequential phases to ensure that later
    /// phases can rely on data collected by earlier ones (e.g., all named types are
    /// available before full type validation).
    ///
    /// Phases:
    /// 1. **Metadata analysis**  
    ///    Validates global metadata nodes (`name`, `servers`, `security`, etc.).
    ///    Checks that each metadata key appears the correct number of times and that
    ///    its value has the expected type/shape.
    ///
    /// 2. **Named type definition lowering**  
    ///    Lowers all top-level type definitions and inserts them into the type arena
    ///    and source map. Detects and reports cyclic type definitions.  
    ///    *Note:* Only basic syntactic validity and name-uniqueness are checked here.
    ///    Full semantic validation is deferred to phase 4 (see below).
    ///
    /// 3. **Route & endpoint lowering**  
    ///    Lowers routes and endpoints, and lowers
    ///    any inline types they contain. Also performs:
    ///    - Route/path uniqueness checks
    ///    - Path-parameter vs. parameters-record consistency
    ///
    /// 4. **Complete type validation**  
    ///    Now that **all** types (named + inline) are present in the arena, performs
    ///    exhaustive semantic validation that requires the full type graph.  
    ///    Examples:
    ///    - All variants of a `union` must be record types
    ///    - Field types exist and are used correctly
    ///    - Enum variants, default values, constraints, etc.
    ///
    ///    Keeping this validation in a separate pass avoids duplicating logic between
    ///    named types and inline types used in endpoints.
    ///
    /// 5. **Example lowering & validation**  
    ///    Lowers `example` blocks, inserts them into the value arena,
    ///    and verifies that each example conforms to its declared type.
    fn analyze(&mut self, root: &ast::Root) {
        self.analyze_metadata(root);

        self.lower_type_definitions(root);

        // A placeholder, just so that warning about unused `Oracle::parse_type`
        // goes away
        // TODO: Remove this in the future.
        if let Some(t) = root.dummy_type() {
            self.lower_type(&t);
        }

        // TODO: Implement the actual analysis
    }

    #[cfg(test)]
    /// Create a new [`Oracle`] without calling analyze on it.
    /// Only used for testing.
    fn new_raw(root: &'a ast::Root) -> Self {
        Self {
            strings: Default::default(),
            values: Default::default(),
            types: Default::default(),
            symbol_table: Default::default(),
            source_map: SourceMap::new(root),
            reports: Default::default(),
        }
    }
}
