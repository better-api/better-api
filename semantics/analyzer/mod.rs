//! Defines core type responsible for semantic analysis.

use std::collections::HashMap;

use better_api_diagnostic::{Report, Severity};
use better_api_syntax::{TextRange, ast};

use crate::mime::MimeArena;
use crate::path::PathId;
use crate::spec::arena::endpoint::{EndpointArena, EndpointId};
use crate::spec::arena::typ::builder::TypeArenaBuilder;
use crate::spec::arena::typ::id::TypeFieldId;
use crate::spec::arena::value::ValueArena;
use crate::spec::metadata::Metadata;
use crate::spec::{Spec, SymbolTable};
use crate::text::{StringId, StringInterner};

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
    let lowerer = SpecLowerer::new(root);
    let lowered_spec = lowerer.lower_spec();
    lowered_spec.validate_spec()
}

type SymbolMap = HashMap<StringId, ast::AstPtr<ast::TypeDefinition>>;

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

pub(crate) struct LoweredSpec {
    spec: Spec,
    range_map: RangeMap,
    reports: Vec<Report>,
}

/// Core type responsible for lowering spec
#[derive(Clone)]
pub(crate) struct SpecLowerer<'a> {
    // Valid spec data being constructed
    strings: StringInterner,
    mimes: MimeArena,
    spec_symbol_table: SymbolTable,

    metadata: Metadata,

    values: ValueArena,
    types: TypeArenaBuilder,
    endpoints: EndpointArena,

    // Mappings used by analyzer during validation.
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

struct Context<'o, 'a> {
    strings: &'o mut StringInterner,
    mimes: &'o mut MimeArena,
    spec_symbol_table: &'o mut SymbolTable,
    symbol_map: &'o mut SymbolMap,
    reports: &'o mut Vec<Report>,
    range_map: &'o mut RangeMap,

    root: &'a ast::Root,
}

impl<'a> SpecLowerer<'a> {
    /// Create a new analyzer.
    ///
    /// Runs semantic analysis on the given AST and creates an analyzer
    /// that can be queried for semantics info.
    fn new(root: &'a ast::Root) -> Self {
        Self {
            strings: Default::default(),
            mimes: Default::default(),
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

    /// Analyzes the complete syntax tree and populates the analyzer arenas.
    fn lower_spec(mut self) -> LoweredSpec {
        self.lower_metadata();

        // Build symbol map and do basic symbol validation
        // - symbol names are unique
        // - no cycles
        self.validate_symbols();

        self.lower_type_definitions();
        self.lower_endpoints_and_routes();

        self.into_lowered_spec()
    }

    fn into_lowered_spec(self) -> LoweredSpec {
        let types = self
            .types
            .finish(&self.spec_symbol_table)
            .expect("SpecLowerer should build type arena correctly");

        LoweredSpec {
            spec: Spec {
                strings: self.strings,
                mimes: self.mimes,
                symbol_table: self.spec_symbol_table,
                metadata: self.metadata,
                values: self.values,
                types,
                endpoints: self.endpoints,
            },
            range_map: self.range_map,
            reports: self.reports,
        }
    }
}

impl LoweredSpec {
    fn validate_spec(mut self) -> AnalyzeResult {
        self.validate_paths();

        if self.reports.iter().any(|r| r.severity == Severity::Error) {
            AnalyzeResult {
                spec: None,
                reports: self.reports,
            }
        } else {
            AnalyzeResult {
                spec: Some(self.spec),
                reports: self.reports,
            }
        }
    }
}
