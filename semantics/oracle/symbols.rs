use smallvec::SmallVec;

use crate::{
    Oracle, StringId,
    typ::{Type, TypeId},
};

/// Result of symbol resolution
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ResolvedSymbol {
    /// No symbol was found
    None,

    /// Symbol successfully resolved into given type.
    Type(TypeId),

    /// Symbol resolution encountered a cycle.
    ///
    /// It returns the name of the cycle's entry point.
    /// For instance, if name resolution follows this path:
    /// ```text
    /// A -> B -> C -> D -> C
    /// ```
    /// then `Cycle(C)` should be returned.
    ///
    /// When returned cycle starts with the symbol that is being
    /// resolved, caller should emit an error report.
    Cycle(StringId),
}

impl<'a> Oracle<'a> {
    /// Resolve a symbol.
    pub(crate) fn resolve(&self, mut name: StringId) -> ResolvedSymbol {
        let mut path = SmallVec::<[StringId; 10]>::new();

        loop {
            if path.contains(&name) {
                return ResolvedSymbol::Cycle(name);
            }
            path.push(name);

            let Some(&next_type) = self.symbol_table.get(&name) else {
                return ResolvedSymbol::None;
            };

            match self.types.get(next_type) {
                Type::Reference(reference) => name = reference,
                _ => return ResolvedSymbol::Type(next_type),
            }
        }
    }
}
