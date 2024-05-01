//! ruspell

mod aff;
mod dic;
mod dictionary;
mod lookup;
mod trie;

pub use dictionary::Dictionary;

/// This interface is used to load multiple dictionaries and efficiently look through them
struct Library {
	pile: Vec<Dictionary>,
}

// TODO: ensure this has an API similar to Dictionary
impl Library {
	fn new(pile: Vec<Dictionary>) -> Self {
		Self { pile }
	}

	fn lookup(&self, word: &str) -> bool {
		self.pile
			.iter()
			.filter_map(|d| d.lookup(word).ok())
			.any(|b| b)
	}
}
