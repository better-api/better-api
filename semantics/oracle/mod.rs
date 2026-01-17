//! Defines core type responsible for semantic analysis.

use std::collections::HashMap;

use better_api_diagnostic::Report;
use better_api_syntax::ast;

use crate::spec::endpoint::EndpointArena;
use crate::spec::typ::TypeArena;
use crate::spec::value::ValueArena;
use crate::spec::{Metadata, SymbolTable};
use crate::string::{StringId, StringInterner};

mod metadata;
mod symbols;
// mod typ;
// mod value;

// #[cfg(test)]
// mod tests;

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
    symbol_map: HashMap<StringId, ast::AstPtr<ast::TypeDefinition>>,
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

    /// Analyzes the complete syntax tree and populates the analyzer arenas.
    fn analyze(&mut self) {
        // TODO: Steps:
        // - analyze type defs (not actual types, just defs), populate self.symbol_map, find cycles
        // - analyze types of type defs, populate spec_symbol_table
        // - analyze routes & endpoints. as you go also analyze and lower types
        self.lower_metadata();

        // self.lower_type_definitions(root);

        // A placeholder, just so that warning about unused `Oracle::parse_type`
        // goes away
        // TODO: Remove this in the future.
        // if let Some(t) = root.dummy_type() {
        //     self.lower_type(&t);
        // }

        // TODO: Implement the actual analysis
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
