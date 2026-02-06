//! Defines core type responsible for semantic analysis.

use std::collections::HashMap;

use better_api_diagnostic::Report;
use better_api_syntax::ast;

use crate::spec::endpoint::EndpointArena;
use crate::spec::typ::TypeArena;
use crate::spec::value::ValueArena;
use crate::spec::{Metadata, SpecContext, SymbolTable};
use crate::string::{StringId, StringInterner};

mod endpoint;
mod metadata;
mod symbols;
mod typ;
mod value;

#[cfg(test)]
mod tests;

type SymbolMap = HashMap<StringId, ast::AstPtr<ast::TypeDefinition>>;

/// Core type responsible for semantic analysis.
#[derive(Clone)]
pub struct Oracle<'a> {
    // Valid spec data being constructed
    strings: StringInterner,
    spec_symbol_table: SymbolTable,

    metadata: Option<Metadata>,

    values: ValueArena,
    types: TypeArena,
    endpoints: EndpointArena,

    // Mappings used by oracle during validation.
    // This allows for partial & invalid spec to be analyzed fully, without
    // being lowered into valid data defined above.
    symbol_map: SymbolMap,
    root: &'a ast::Root,

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
            spec_symbol_table: Default::default(),

            metadata: None,

            values: Default::default(),
            types: Default::default(),
            endpoints: Default::default(),

            symbol_map: Default::default(),
            root,

            reports: Default::default(),
        };

        oracle.analyze();
        oracle
    }

    /// Get semantic problems.
    pub fn reports(&self) -> &[Report] {
        &self.reports
    }

    /// Get [view](SpecContext) over a spec.
    fn spec_ctx<'s>(&'s self) -> SpecContext<'s> {
        SpecContext {
            strings: &self.strings,
            symbol_table: &self.spec_symbol_table,
            values: &self.values,
            types: &self.types,
            endpoints: &self.endpoints,
        }
    }

    /// Analyzes the complete syntax tree and populates the analyzer arenas.
    fn analyze(&mut self) {
        self.lower_metadata();

        // Build symbol map and do basic symbol validation
        // - symbol names are unique
        // - no cycles
        self.validate_symbols();

        self.lower_type_definitions();

        self.lower_endpoints();
    }

    #[cfg(test)]
    /// Create a new [`Oracle`] without calling analyze on it.
    /// Only used for testing.
    fn new_raw(root: &'a ast::Root) -> Self {
        Self {
            strings: Default::default(),
            spec_symbol_table: Default::default(),

            metadata: None,

            values: Default::default(),
            types: Default::default(),
            endpoints: Default::default(),

            symbol_map: Default::default(),
            root,

            reports: Default::default(),
        }
    }
}
