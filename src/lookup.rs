//! Logic of the [`Dictionary`] to validate a word

use crate::{
	aff::{Affix, Prefix, Suffix},
	dic::{Casing, Stem},
	Dictionary,
};
use std::{fmt, iter};

/// Maximum word length to check
//
/// This limit is the same in `Hunspell`, it's arbitrary
const WORD_LOOKUP_MAX_LENGTH: usize = 100;

/// Informs why a malformed word has been refused or failed to lookup
#[derive(Debug, thiserror::Error)]
pub enum LookupError {
	/// Word is too long and makes no sense to check
	#[error("word should not be over {} bytes long", WORD_LOOKUP_MAX_LENGTH)]
	WordTooLong,
}

// TODO: move all lookup logic in `Lookup` struct, add small caches for homonym and trie lookup?

/// Return the first item of the given iterator but can also fully evaluate
/// further the iterator for debugging purposes
fn eager_iter_first<T>(mut iter: impl Iterator<Item = T>) -> Option<T> {
	let res = iter.next();

	if cfg!(feature = "fully_evaluate") {
		let mut iter = iter.peekable();
		if iter.peek().is_some() {
			log::debug!("continuing valid forms evaluation");
			iter.for_each(drop);
		}
	}

	res
}

/// Methods for querying the dictionary
impl Dictionary {
	/// # Errors
	/// Check [`LookupError`] to see all the ways this function breaks
	pub fn lookup(&self, word: &str) -> Result<bool, LookupError> {
		// TODO: check max word length (hunspell's 360)
		if word.len() > WORD_LOOKUP_MAX_LENGTH {
			return Err(LookupError::WordTooLong);
		}

		// Get plain forbidden words out of the way
		if let Some(fw_flag) = self.aff.additional_flags.forbidden_word {
			if self.dic.homonyms(word).any(|s| s.flags.has_flag(&fw_flag)) {
				return Ok(false);
			}
		}

		log::debug!("lookup {word:?}");

		// Trim blanks around word
		let word = word.trim();
		// Remove abbreviation dot at the the end
		let (word, _abbreviation) = match &word.chars().next_back() {
			Some('.') => (&word[..word.len() - 1], true),
			_ => (word, false),
		};
		// TODO: add dot back
		let word = word.trim_end_matches(|c: char| c == '.');

		log::debug!("trimmed {word:?}");

		// TODO: do we care about alloc for small strings?
		let mut word = word.to_owned();

		// Input conversion based on the `ICONV` table
		self.aff.options.input_conversion.convert(&mut word);
		// Erase ignored characters as the specified by the `IGNORE` table
		self.aff.options.ignore.erase(&mut word);

		log::debug!("sanitized {word:?}");

		// Check for numbers separated with dashes, dots and commas
		if Self::is_number(&word) {
			return Ok(true);
		}

		let valid_forms = self
			.break_word(&word)
			.inspect(|wb| log::debug!("break form {wb:?}"))
			.filter_map(|forms| {
				forms
					.into_iter()
					.find_map(|p| eager_iter_first(self.forms(p)))
			});

		Ok(eager_iter_first(valid_forms).is_some())
	}

	/// Break the input word in every possible way as defined by the `BREAK` directive
	fn break_word<'a>(&self, word: &'a str) -> impl Iterator<Item = Vec<&'a str>> + 'a {
		let break_possibilities = iter::once(vec![word]);

		for pattern in &self.aff.options.compound_split_points {
			todo!()
		}

		break_possibilities
	}

	/// Return every valid form of this word as is could be interpreted by the dictionary
	fn forms<'a>(&'a self, word: &'a str) -> impl Iterator<Item = WordForm> + 'a {
		let word_casing = Casing::guess(word);

		// TODO: add capitalisation option
		let capitalisation = true;
		let (case, forms) = if capitalisation {
			let variants = word_casing.variants(word.to_owned());
			(word_casing, variants)
		} else {
			(word_casing, vec![word.to_owned()])
		};

		log::debug!("casing guess: {case}");

		forms
			.into_iter()
			.inspect(|word| log::debug!("casing {word:?}"))
			.flat_map(|form| {
				let affix_forms = self.affix_forms(&form).map(WordForm::Affix);
				let compound_forms = self.compound_forms(&form).map(WordForm::Compound);

				affix_forms
					.chain(compound_forms)
					// TODO: find a way to avoid collecting all forms
					.collect::<Vec<_>>()
					.into_iter()
			})
			.inspect(|f| log::debug!("valid form {f}"))
	}

	/// Return every valid affix form of this word as interpreted by the dictionary
	fn affix_forms<'a>(&'a self, word: &'a str) -> impl Iterator<Item = AffixForm> + 'a {
		// For every form that could exist, we check it's validity
		self.produce_affix_forms(word)
			.inspect(|f| log::debug!("affix form {f}"))
			// TODO: forbidden
			.filter(|form| {
				// Only accept words that appear in the dictionary
				self.dic
					.homonyms(&form.stem)
					.any(|stem| self.is_valid_affix_form(form, stem))
			})
	}

	/// Whether the given [`AffixForm`] would match current [`Stem`] definition
	fn is_valid_affix_form(&self, form: &AffixForm, entry: &Stem) -> bool {
		// TODO: no suggest flag

		if let Some(pfx) = &form.prefix {
			if !entry.flags.has_flag(&pfx.flag) {
				log::debug!("does not have corresponding prefix flag");
				return false;
			}
		}

		if let Some(sfx) = &form.suffix {
			if !entry.flags.has_flag(&sfx.flag) {
				log::debug!("does not have corresponding suffix flag");
				return false;
			}
		}
		if let Some(kc_flag) = self.aff.additional_flags.keep_case {
			if entry.flags.has_flag(&kc_flag) {
				// TODO: check entry.case == Casing::No;
				// log::debug!("does not have corresponding keepcase flag");
			}
		}

		true
	}

	/// Produce every possible [`AffixForm`], it will be validated by [`Dictionary::is_valid_affix_form`] after
	fn produce_affix_forms<'a>(&'a self, word: &'a str) -> impl Iterator<Item = AffixForm> + '_ {
		// whole word without affixes is one possible form
		let whole_word = AffixForm::new(word, None, None);

		iter::once(whole_word)
			.chain(self.produce_prefix_and_cross_forms(word))
			.chain(self.produce_suffix_forms(word, None))
	}

	// this is an alternative impl to spylls that only searches the affixes once to then combine them
	// TODO: bench and choose
	/// Produce every possible [`AffixForm`], it will be validated by [`Dictionary::is_valid_affix_form`] after
	#[cfg(debug_assertions)]
	fn produce_affix_forms_two<'a>(
		&'a self,
		word: &'a str,
	) -> impl Iterator<Item = AffixForm> + '_ {
		let whole_word = AffixForm::new(word, None, None);

		let mut forms = vec![whole_word];

		// TODO: i just supposed there would be less prefixes, check
		let prefixes = self.aff.prefix_index.get_all(word);

		for prefix in &prefixes {
			forms.push(AffixForm::new(word, Some((*prefix).clone()), None));
		}

		for suffix in self.aff.suffix_index.get_all(word) {
			forms.push(AffixForm::new(word, None, Some(suffix.clone())));

			let suffix_allowed = true;
			if suffix_allowed && suffix.cross_product {
				for prefix in prefixes.iter().filter(|p| p.cross_product) {
					forms.push(AffixForm::new(
						word,
						Some((*prefix).clone()),
						Some(suffix.clone()),
					));
				}
			}
		}

		// TODO: iterify
		forms.into_iter()
	}

	/// Produce prefix [`AffixForm`]s and cross product [`AffixForm`]s which has both a prefix and a suffix
	fn produce_prefix_and_cross_forms<'a>(
		&'a self,
		word: &'a str,
	) -> impl Iterator<Item = AffixForm> + 'a {
		self.aff
			.prefix_index
			.get_all(word)
			.into_iter()
			.map(|prefix| (prefix, AffixForm::new(word, Some(prefix.clone()), None)))
			.flat_map(move |(prefix, form)| {
				// TODO: filter prefixes that don't match condition
				if prefix.cross_product {
					iter::once(form.clone())
						.chain(self.produce_suffix_forms(word, Some(form)))
						.collect()
				} else {
					vec![form]
				}
			})
	}

	/// Produce suffix [`AffixForm`]s
	fn produce_suffix_forms<'a>(
		&'a self,
		word: &'a str,
		// pass a prefix to form complex affix forms, see crossproduct
		prefix: Option<AffixForm>,
	) -> impl Iterator<Item = AffixForm> + 'a {
		let collect = word.chars().rev().collect::<String>();
		// TODO: GG
		let collect = Box::leak(Box::new(collect));

		let check_cross_product = prefix.is_some();

		self.aff
			.suffix_index
			.get_all(collect)
			.into_iter()
			.filter(move |s| {
				if check_cross_product {
					s.cross_product
				} else {
					true
				}
			})
			// TODO: check for sub on s.add replace with strip
			.filter(|s| {
				let mut word = word.strip_suffix(&s.add).unwrap().to_owned();
				word.push_str(&s.strip);

				s.condition.clone().map_or(true, |r| r.is_match(&word))
			})
			.map(move |suffix| {
				prefix.as_ref().map_or_else(
					|| AffixForm::new(word, None, Some(suffix.clone())),
					|prefix| {
						let mut prefix = prefix.clone();
						prefix.suffix = Some(suffix.clone());
						let strip_suffix = prefix.stem.strip_suffix(&suffix.add).unwrap();
						prefix.stem = format!("{strip_suffix}{}", suffix.strip);
						prefix
					},
				)
			})
	}

	// Compound Forms

	fn compound_forms<'a>(&'a self, word: &'a str) -> impl Iterator<Item = CompoundForm> + 'a {
		vec![].into_iter()
	}

	/// Checks if `word` is only composed of digits and separators (`-,.`)
	/// that don't follow each other.
	fn is_number(word: &str) -> bool {
		let mut previous_is_sep = false;

		if word.is_empty() {
			return false;
		}

		for char_ in word.chars() {
			match char_ {
				'0'..='9' => previous_is_sep = false,
				'-' | '.' | ',' if !previous_is_sep => previous_is_sep = true,
				_ => return false,
			}
		}

		true
	}
}

/// A valid composed word form as understood by a diticonaru
#[derive(Debug)]
enum WordForm {
	/// An affix
	Affix(AffixForm),
	/// A compound
	Compound(CompoundForm),
}

impl fmt::Display for WordForm {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Affix(a) => write!(f, "AffixForm{a}"),
			Self::Compound(c) => write!(f, "CompoundForm{c}"),
		}
	}
}

/// A form a word can be split into with a optional prefix and an optional suffix
#[derive(Debug, Clone)]
struct AffixForm {
	/// Source text
	text: String,

	// TODO: slices of [`text`]
	/// stem as it may exists in dictionary
	stem: String,

	/// a optional prefix
	prefix: Option<Affix<Prefix>>,
	/// a optional second prefix
	prefix_second: Option<Affix<Prefix>>,

	/// a optional suffix
	suffix: Option<Affix<Suffix>>,
	/// a optional second suffix
	suffix_second: Option<Affix<Suffix>>,
}

impl AffixForm {
	// TODO: test case up there to see what happens when there's a form where prefix and suffix overlap
	/// # Panics
	///
	/// Panics if it could not properly strip affixes to get the stem
	fn new(word: &str, prefix: Option<Affix<Prefix>>, suffix: Option<Affix<Suffix>>) -> Self {
		let stem = match (&prefix, &suffix) {
			(Some(prefix), Some(suffix)) => {
				let stem = word
					.strip_prefix(&prefix.add)
					.expect("word should have prefix");
				let stem = stem
					.strip_suffix(&suffix.add)
					.expect("word should have suffix");

				format!("{}{stem}{}", prefix.strip, suffix.strip)
			}
			(Some(prefix), None) => {
				let stem = word
					.strip_prefix(&prefix.add)
					.expect("word should have prefix");

				format!("{}{stem}", prefix.strip)
			}

			(None, Some(suffix)) => {
				let stem = word
					.strip_suffix(&suffix.add)
					.expect("word should have suffix");

				format!("{stem}{}", suffix.strip)
			}
			(None, None) => word.to_owned(),
		};

		Self {
			text: word.to_owned(),
			stem,
			prefix,
			prefix_second: None,
			suffix,
			suffix_second: None,
		}
	}
}

impl fmt::Display for AffixForm {
	/// Must look like
	/// AffixForm(text = prefix + stem + suffix)
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "[{} = ", self.text)?;

		if let Some(prefix) = &self.prefix {
			write!(f, "{prefix} + ")?;
		}

		write!(f, "{}", self.stem)?;

		if let Some(suffix) = &self.suffix {
			write!(f, " + {suffix}")?;
		}

		write!(f, "]")
	}
}

#[derive(Debug)]
struct CompoundForm {}

impl fmt::Display for CompoundForm {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "[]")
	}
}
