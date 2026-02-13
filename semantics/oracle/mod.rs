//! Defines core type responsible for semantic analysis.

use std::collections::HashMap;

use better_api_diagnostic::Report;
use better_api_syntax::{TextRange, ast};

use crate::path::PathId;
use crate::spec::endpoint::EndpointArena;
use crate::spec::typ::{RootTypeId, TypeArena, TypeFieldId, TypeId};
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

    // Range mapping for some of the semantic elements.
    //
    // Some of the checks are done after lowering the types, because
    // it's easier this way. We use this map to store the range information
    // so that the checks can emit nice reports.
    range_map: RangeMap,
}

#[derive(Clone, Default)]
struct RangeMap {
    /// Path to range mapping.
    ///
    /// Note that path is made up of multiple parts. PathId points to the last
    /// part of the path. This mapping maps last path part to its range.
    path: HashMap<PathId, TextRange>,

    /// Map record and union field names to their ranges.
    field_name: HashMap<TypeFieldId, TextRange>,

    /// Maps endpoint's path params attribute to their ranges.
    ///
    /// This is used to emit diagnostics for endpoint's path params, ie:
    /// ```text
    /// GET {
    ///     path: {
    ///     ^^^^ This is a problem
    ///       foo: string
    ///       ...
    ///     }
    /// }
    /// ```
    endpoint_path_attribute_name: HashMap<RootTypeId, TextRange>,
}

struct Context<'o, 'a> {
    strings: &'o mut StringInterner,
    spec_symbol_table: &'o mut SymbolTable,
    symbol_map: &'o mut SymbolMap,
    reports: &'o mut Vec<Report>,
    range_map: &'o mut RangeMap,

    root: &'a ast::Root,
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

            range_map: Default::default(),
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
        // TODO: Lower routes
        self.validate_paths();
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

            range_map: Default::default(),
        }
    }
}
