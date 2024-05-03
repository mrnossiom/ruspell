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

/// Methods for querying the dictionary
impl Dictionary {
	/// # Errors
	/// Check [`LookupError`] to see all the ways this function breaks
	pub fn lookup(&self, word: &str) -> Result<bool, LookupError> {
		// TODO: check max word length (hunspell's a 100)
		if word.len() > WORD_LOOKUP_MAX_LENGTH {
			return Err(LookupError::WordTooLong);
		}

		// Get plain forbidden words out of the way
		if let Some(ff) = &self.aff.additional_flags.forbidden_word {
			if self.dic.homonyms(word).any(|s| s.flags.contains(ff)) {
				return Ok(false);
			}
		}

		let word = self.aff.options.input_conversion.convert(word);

		let word = word.trim_matches(|c: char| c.is_whitespace() && c == '.');
		// TODO: make input conversion

		// TODO: clean word (e.g strip blanks)

		// TODO: detect capitalization type

		// TODO: check for numbers separated with dashes, dots and commas (sounds like a job for nom)

		// TODO: check word for all capitalisation types

		// TODO: split word
		let parts = vec![word];

		let x = Ok(parts
			.into_iter()
			.flat_map(|p| self.forms(p))
			.next()
			.is_some());
		x
	}

	/// Return every valid form of this word as is could be interpreted by the dictionary
	fn forms<'a>(&'a self, word: &'a str) -> impl Iterator<Item = WordForm> + 'a {
		// capitalisation
		let (cap, forms) = if false {
			todo!("check form for all caps type");
		// return Err(LookupError::WordTooLong);
		} else {
			(Casing::guess(word), vec![word])
		};

		forms.into_iter().flat_map(|form| {
			self.affix_forms(form)
				.into_iter()
				.map(WordForm::Affix)
				.chain(
					self.compound_forms(form)
						.into_iter()
						.map(WordForm::Compound),
				)
		})
	}

	/// Return every valid affix form of this word as interpreted by the dictionary
	fn affix_forms(&self, word: &str) -> Vec<AffixForm> {
		// TODO: iterify that
		let mut forms = vec![];

		// For every form that could exist, we check it's validity
		for form in self.produce_affix_forms(word).inspect(|w| println!("{w}")) {
			// TODO: forbidden

			// Only accept words that appear in the dictionary
			for stem in self.dic.homonyms(&form.stem) {
				if self.is_valid_affix_form(&form, stem) {
					// TODO: no, pushed two times
					forms.push(form.clone());
				}
			}
		}
		forms
	}

	/// Wether the given [`AffixForm`] would match current [`Stem`] definition
	fn is_valid_affix_form(&self, form: &AffixForm, entry: &Stem) -> bool {
		// TODO: no suggest flag

		if !form
			.prefix
			.as_ref()
			.map_or(true, |p| entry.flags.contains(&p.flag))
		{
			return false;
		}

		if !form
			.suffix
			.as_ref()
			.map_or(true, |p| entry.flags.contains(&p.flag))
		{
			return false;
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

		// iter::once(whole_word)
		// 	.chain(self.produce_prefix_and_cross_forms(word))
		// 	.chain(self.produce_suffix_forms(word, None))

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
			.inspect(|s| println!("{s}"))
			// TODO: check for sub on s.add replace with strip
			.filter(|s| s.condition.clone().map_or(true, |r| r.is_match(word)))
			.inspect(|s| println!("{s}"))
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

	fn compound_forms(&self, word: &str) -> Vec<CompoundForm> {
		vec![]
	}
}

enum WordForm {
	Affix(AffixForm),
	Compound(CompoundForm),
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
	/// a optional suffix
	suffix: Option<Affix<Suffix>>,
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
			suffix,
		}
	}
}

impl fmt::Display for AffixForm {
	/// Must look like
	/// AffixForm(text = prefix + stem + suffix)
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "AffixForm[{} = ", self.text)?;

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

struct CompoundForm {}
