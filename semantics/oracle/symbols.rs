use better_api_diagnostic::{Label, Report, Severity};
use better_api_syntax::TextRange;
use better_api_syntax::ast::{self, AstNode, AstPtr};
use smallvec::SmallVec;

use crate::Oracle;
use crate::string::StringId;

type ResolvePath = SmallVec<[StringId; 10]>;

/// Result of symbol resolution
#[derive(Debug, Clone)]
pub(crate) enum ResolvedSymbol {
    /// Symbol with name inside this type is not in the symbols table.
    ///
    /// For example, when resolving symbol `Foo` in:
    /// ```text
    /// type Foo: Bar
    /// type Bar: Baz // Baz is not defined anywhere
    /// ```
    /// resolution should return `Missing(Baz)`
    ///
    /// Code calling symbol resolution should report a missing error, if symbols inside
    /// `Missing` is the same as name that is being resolved.
    Missing { name: StringId, range: TextRange },

    /// Symbol resolution encountered a cycle.
    Cycle,

    /// Symbol successfully resolved into given type.
    Type(ast::Type),
}

impl<'a> Oracle<'a> {
    /// Resolve a symbol.
    pub(crate) fn resolve(&self, mut name: StringId, mut range: TextRange) -> ResolvedSymbol {
        let mut path = ResolvePath::new();

        loop {
            if path.contains(&name) {
                return ResolvedSymbol::Cycle;
            }
            path.push(name);

            let Some(next_type_def) = self
                .symbol_map
                .get(&name)
                .map(|ptr| ptr.to_node(self.root.syntax()))
            else {
                return ResolvedSymbol::Missing { name, range };
            };

            let Some(next_type) = next_type_def.typ() else {
                return ResolvedSymbol::Missing { name, range };
            };

            match next_type {
                ast::Type::TypeRef(reference) => {
                    let next_name = reference.name();
                    let next_id = self.strings.get(next_name.text());
                    if let Some(next) = next_id {
                        name = next;
                        range = reference.syntax().text_range();
                    } else {
                        return ResolvedSymbol::Missing { name, range };
                    }
                }
                typ => return ResolvedSymbol::Type(typ),
            }
        }
    }

    /// Builds symbol map and validates redefinitions and cycles.
    pub(crate) fn validate_symbols(&mut self) {
        self.build_symbol_map();
        self.report_symbol_cycles();
    }

    fn build_symbol_map(&mut self) {
        for def in self.root.type_definitions() {
            let Some(name) = def.name() else {
                continue;
            };

            // TODO: Validate name with text::something
            let name_id = self.strings.get_or_intern(name.text());
            if let Some(original_def_ptr) = self.symbol_map.get(&name_id) {
                let original_def = original_def_ptr.to_node(self.root.syntax());

                // If symbol table contains the name already, we should also have a valid type def
                // node, so expects should be fine.
                let original_range = original_def
                    .name()
                    .expect("name of original type def should exist")
                    .text_range();

                let name_str = name.text();
                let range = name.text_range();

                self.reports.push(
                    Report::error(format!("name `{name_str}` is defined multiple times"))
                        .add_label(Label::primary(
                            format!("name `{name_str}` is defined multiple times"),
                            range.into(),
                        ))
                        .add_label(Label::secondary(
                            format!("name `{name_str}` is first defined here"),
                            original_range.into(),
                        )),
                );
            } else {
                self.symbol_map.insert(name_id, AstPtr::new(&def));
            }
        }
    }

    /// Run cycle detection on defined symbols and report them.
    fn report_symbol_cycles(&mut self) {
        let mut path = ResolvePath::new();
        let mut reports = vec![];
        for (name, typ_ptr) in self.symbol_map.iter() {
            path.clear();
            path.push(*name);

            reports.clear();

            let typ_def = typ_ptr.to_node(self.root.syntax());
            let Some(typ) = typ_def.typ() else {
                continue;
            };

            self.report_symbol_cycles_aux(&typ, &mut path, &mut reports);
            self.reports.extend_from_slice(&reports);
        }
    }

    /// Helper function for reporting symbol cycles.
    ///
    /// It recursively tries to resolve all references of a given type.
    /// If a cycle is found, it is reported.
    fn report_symbol_cycles_aux(
        &self,
        typ: &ast::Type,
        path: &mut ResolvePath,
        reports: &mut Vec<Report>,
    ) {
        match typ {
            // Primitive types are always valid.
            ast::Type::TypeI32(_)
            | ast::Type::TypeI64(_)
            | ast::Type::TypeU32(_)
            | ast::Type::TypeU64(_)
            | ast::Type::TypeF32(_)
            | ast::Type::TypeF64(_)
            | ast::Type::TypeDate(_)
            | ast::Type::TypeTimestamp(_)
            | ast::Type::TypeBool(_)
            | ast::Type::TypeString(_)
            | ast::Type::TypeFile(_) => (),

            // Enum can't have cycles. Type is restricted to non-cyclable types,
            // and members are values. Checking type to be correct (implies non-cyclable)
            // already reports an error. Reporting both "invalid enum type" and "enum has cycles" error
            // would be weird.
            ast::Type::Enum(_) => (),

            // Option and array are terminating types. You can always end recursion with empty
            // array `[]` or empty option `null`. For instance
            // ```text
            // type Foo: [Foo]
            // ```
            // is valid, because `[]` is valid value of type Foo (is an array and all children are
            // of type Foo because there are no children).
            // Also more interesting values are valid Foo, but useless, ie `[[[], []], []]`
            ast::Type::TypeOption(_) | ast::Type::TypeArray(_) => (),

            ast::Type::Record(record) => {
                for field in record.fields() {
                    if let Some(field_type) = field.typ() {
                        self.report_symbol_cycles_aux(&field_type, path, reports);
                    }
                }
            }
            ast::Type::Union(union) => {
                for field in union.fields() {
                    if let Some(field_type) = field.typ() {
                        self.report_symbol_cycles_aux(&field_type, path, reports);
                    }
                }
            }

            ast::Type::TypeResponse(resp) => {
                if let Some(headers) = resp.headers()
                    && let Some(typ) = headers.typ()
                {
                    self.report_symbol_cycles_aux(&typ, path, reports);
                }

                if let Some(body) = resp.body()
                    && let Some(typ) = body.typ()
                {
                    self.report_symbol_cycles_aux(&typ, path, reports);
                }
            }

            ast::Type::TypeRef(reference) => {
                let next_name = reference.name();
                let Some(name) = self.strings.get(next_name.text()) else {
                    return;
                };

                // References back to the first element in path, so we want to report it.
                if path.first() == Some(&name) {
                    let report = self.construct_cycle_report(path);
                    reports.push(report);
                    return;
                }

                // References to somewhere in path (but not the beginning), so we don't want to
                // report it, but still want to stop the recursion.
                if path.contains(&name) {
                    return;
                }

                // Resolve the symbol and call recursively.
                if let Some(typ_ptr) = self.symbol_map.get(&name) {
                    let typ_def = typ_ptr.to_node(self.root.syntax());
                    let Some(typ) = typ_def.typ() else {
                        return;
                    };

                    path.push(name);
                    self.report_symbol_cycles_aux(&typ, path, reports);
                    path.pop();
                }
            }
        }
    }

    /// Helper method to construct a report about an invalid cycle.
    fn construct_cycle_report(&self, path: &ResolvePath) -> Report {
        let names =
            path.iter()
                .map(|id| self.strings.resolve(*id))
                .fold(String::new(), |mut acc, elt| {
                    if !acc.is_empty() {
                        acc.push_str(", ");
                    }

                    acc.push('`');
                    acc.push_str(elt);
                    acc.push('`');
                    acc
                });

        let msg = format!("types {names} create an infite recursion");

        let labels: Vec<_> = path
            .iter()
            .enumerate()
            .map(|(idx, id)| {
                let def_ptr = self.symbol_map.get(id)
                    .expect("symbol in cycle path should have been registered in source_map during type definition parsing");
                let def = def_ptr.to_node(self.root.syntax());

                let range = def
                    .name()
                    .expect("type definition should have a name - only named types are stored in source_map")
                    .text_range();

                if idx == 0 {
                    Label::primary(msg.clone(), range.into())
                } else {
                    let name = self.strings.resolve(*id);

                    Label::secondary(format!("type `{name}` defined here"), range.into())
                }
            })
            .collect();

        Report {
            severity: Severity::Error,
            title: msg,
            labels,
            note: Some(
                "help: type cycles have to end with an Array or Option to avoid infinite recursion"
                    .to_string(),
            ),
        }
    }
}

/// Construct a report for missing symbol.
pub(crate) fn report_missing(name: &str, range: TextRange) -> Report {
    Report::error(format!("undefined symbol `{name}`")).add_label(Label::primary(
        format!("symbol `{name}` is not defined"),
        range.into(),
    ))
}
