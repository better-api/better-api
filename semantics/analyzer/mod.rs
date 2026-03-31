//! Defines core type responsible for semantic analysis.

use std::collections::HashMap;

use better_api_diagnostic::{Report, Severity};
use better_api_syntax::{TextRange, ast};

use crate::path::PathId;
use crate::spec::endpoint::{EndpointArena, EndpointId};
use crate::spec::typ::{TypeArena, TypeFieldId};
use crate::spec::value::ValueArena;
use crate::spec::{Metadata, Spec, SymbolTable};
use crate::text::{StringId, StringInterner};

#[cfg(test)]
use crate::spec::SpecContext;

mod compare;
mod endpoint;
mod metadata;
mod symbols;
mod typ;
mod value;

#[cfg(test)]
mod tests;

/// Result returned by the analyzer.
///
/// If the whole analysis was successful a final [`Spec`] is returned,
/// otherwise spec is `None`. Analysis is successful if there are no reports,
/// or the reports are only warnings.
///
/// In other words, if any of the reports is an error, returned spec is None.
pub struct AnalyzeResult {
    pub spec: Option<Spec>,
    pub reports: Vec<Report>,
}

/// Analyzes the AST and returns all reports and semantic representation of the Spec.
pub fn analyze(root: &ast::Root) -> AnalyzeResult {
    let mut analyzer = Analyzer::new(root);
    analyzer.analyze();
    analyzer.into_spec()
}

type SymbolMap = HashMap<StringId, ast::AstPtr<ast::TypeDefinition>>;

/// Core type responsible for semantic analysis.
#[derive(Clone)]
pub(crate) struct Analyzer<'a> {
    // Valid spec data being constructed
    strings: StringInterner,
    spec_symbol_table: SymbolTable,

    metadata: Metadata,

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
    endpoint_path_attribute_name: HashMap<EndpointId, TextRange>,
}

struct Context<'o, 'a> {
    strings: &'o mut StringInterner,
    spec_symbol_table: &'o mut SymbolTable,
    symbol_map: &'o mut SymbolMap,
    reports: &'o mut Vec<Report>,
    range_map: &'o mut RangeMap,

    root: &'a ast::Root,
}

impl<'a> Analyzer<'a> {
    /// Create a new oracle.
    ///
    /// Runs semantic analysis on the given AST and creates an oracle
    /// that can be queried for semantics info.
    fn new(root: &'a ast::Root) -> Self {
        Self {
            strings: Default::default(),
            spec_symbol_table: Default::default(),

            metadata: Default::default(),

            values: Default::default(),
            types: Default::default(),
            endpoints: Default::default(),

            symbol_map: Default::default(),
            root,

            reports: Default::default(),

            range_map: Default::default(),
        }
    }

    /// Get [view](SpecContext) over a spec.
    #[cfg(test)]
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
        self.lower_endpoints_and_routes();
        self.validate_paths();
    }

    fn into_spec(self) -> AnalyzeResult {
        // Construct the final spec
        if self.reports.iter().any(|r| r.severity == Severity::Error) {
            AnalyzeResult {
                spec: None,
                reports: self.reports,
            }
        } else {
            let spec = Spec {
                strings: self.strings,
                symbol_table: self.spec_symbol_table,
                metadata: self.metadata,
                values: self.values,
                types: self.types,
                endpoints: self.endpoints,
            };

            AnalyzeResult {
                spec: Some(spec),
                reports: self.reports,
            }
        }
    }
}
