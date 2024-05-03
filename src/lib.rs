//! ruspell

mod aff;
mod dic;
mod trie;

pub mod dictionary;
pub mod lookup;
pub mod suggest;

pub use dictionary::Dictionary;

/// Piling dictinaries makes it easier to search through them one after another
struct Pile {
	/// Dictionaries constituting the pile in order
	///
	/// Order is important because the first dictinary that understands the word,
	/// has the most importance in suggestions.
	dicts: Vec<Dictionary>,
}

// TODO: ensure this has an API similar to Dictionary
impl Pile {
	/// Initialize a new [`Pile`]
	fn new(dicts: Vec<Dictionary>) -> Self {
		Self { dicts }
	}

	/// Lookup a word, going through all dictinaries in order
	fn lookup(&self, word: &str) -> bool {
		self.dicts
			.iter()
			.filter_map(|dict| dict.lookup(word).ok())
			.any(|exists| exists)
	}
}
