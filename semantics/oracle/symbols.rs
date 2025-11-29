use better_api_diagnostic::{Label, Report, Severity};
use smallvec::SmallVec;

use crate::Oracle;
use crate::string::StringId;
use crate::typ::{Type, TypeId, Union};

type ResolvePath = SmallVec<[StringId; 10]>;

/// Result of symbol resolution
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ResolvedSymbol<'a> {
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
    Missing(StringId),

    /// Symbol resolution encountered a cycle.
    Cycle,

    /// Symbol successfully resolved into given type.
    Type { id: TypeId, typ: Type<'a> },
}

impl<'a> Oracle<'a> {
    /// Resolve a symbol.
    pub(crate) fn resolve<'o>(&'o self, mut name: StringId) -> ResolvedSymbol<'o> {
        let mut path = ResolvePath::new();

        loop {
            if path.contains(&name) {
                return ResolvedSymbol::Cycle;
            }
            path.push(name);

            let Some(&next_type) = self.symbol_table.get(&name) else {
                return ResolvedSymbol::Missing(name);
            };

            match self.types.get(next_type) {
                Type::Reference(reference) => name = reference,
                typ => return ResolvedSymbol::Type { id: next_type, typ },
            }
        }
    }

    /// Run cycle detection on defined symbols and report them.
    pub(crate) fn report_symbol_cycles(&mut self) {
        let mut path = ResolvePath::new();
        let mut reports = vec![];
        for (name, type_id) in self.symbol_table.iter() {
            path.clear();
            path.push(*name);

            reports.clear();

            self.report_symbol_cycles_aux(*type_id, &mut path, &mut reports);
            self.reports.extend_from_slice(&reports);
        }
    }

    /// Helper function for reporting symbol cycles.
    ///
    /// It recursively tries to resolve all references of a given type.
    /// If a cycle is found, it is reported.
    fn report_symbol_cycles_aux(
        &self,
        type_id: TypeId,
        path: &mut ResolvePath,
        reports: &mut Vec<Report>,
    ) {
        match self.types.get(type_id) {
            // Primitive types are always valid.
            Type::I32
            | Type::I64
            | Type::U32
            | Type::U64
            | Type::F32
            | Type::F64
            | Type::Date
            | Type::Timestamp
            | Type::Bool
            | Type::String
            | Type::File => (),

            // Enum can't have cycles. Type is restricted to non-cyclable types,
            // and everything else is a value.
            Type::Enum(_) => (),

            // Option and array are terminating types. You can always end recursion with empty
            // array `[]` or empty option `null`. For instance
            // ```text
            // type Foo: [Foo]
            // ```
            // is valid, because `[]` is valid value of type Foo (is an array and all children are
            // of type Foo because there are no children).
            // Also more interesting values are valid Foo, but useless, ie `[[[], []], []]`
            Type::Option(_) | Type::Array(_) => (),

            Type::Record(fields) | Type::Union(Union { fields, .. }) => {
                for field in fields {
                    self.report_symbol_cycles_aux(field.id.type_id(), path, reports);
                }
            }

            Type::Response(resp) => {
                if let Some(headers) = resp.headers {
                    self.report_symbol_cycles_aux(headers.id, path, reports);
                }

                if let Some(body) = resp.body {
                    self.report_symbol_cycles_aux(body.id, path, reports);
                }
            }

            Type::Reference(name) => {
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
                if let Some(type_id) = self.symbol_table.get(&name) {
                    path.push(name);
                    self.report_symbol_cycles_aux(*type_id, path, reports);
                    path.pop();
                }
            }
        }
    }

    /// Helper method to construct a report about an invalid cycle.
    fn construct_cycle_report(&self, path: &ResolvePath) -> Report {
        let names =
            path.iter()
                .map(|id| self.strings.get(*id))
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
                let def = self
                    .source_map
                    .get_type_definition(*id)
                    .expect("symbol in cycle path should have been registered in source_map during type definition parsing");
                let range = def
                    .name()
                    .expect("type definition should have a name - only named types are stored in source_map")
                    .text_range();

                if idx == 0 {
                    Label::primary(msg.clone(), range.into())
                } else {
                    let name = self.strings.get(*id);

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
