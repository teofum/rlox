use string_interner::{DefaultBackend, StringInterner};

pub type Symbol = string_interner::symbol::DefaultSymbol;

pub struct Lookups {
    string_interner: StringInterner<DefaultBackend>,
}

impl Default for Lookups {
    fn default() -> Self {
        Self::new()
    }
}

impl Lookups {
    pub fn new() -> Self {
        Self { string_interner: StringInterner::default() }
    }

    pub fn get(&mut self, str: &str) -> Symbol {
        self.string_interner.get_or_intern(str)
    }
}