/// Id of an interned string.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct StringId(string_interner::DefaultSymbol);

#[derive(Default, Clone)]
pub struct StringInterner(string_interner::DefaultStringInterner);

impl StringInterner {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Default::default()
    }

    /// Interns a given string
    pub fn get_or_intern(&mut self, string: impl AsRef<str>) -> StringId {
        let id = self.0.get_or_intern(string);
        StringId(id)
    }

    /// Return symbol for given string, if any
    pub fn get(&self, string: impl AsRef<str>) -> Option<StringId> {
        self.0.get(string).map(StringId)
    }

    /// Returns the string for given symbol
    pub fn resolve(&self, id: StringId) -> &str {
        self.0
            .resolve(id.0)
            .expect("interend string should be resolvable")
    }
}
