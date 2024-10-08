//! Logic to parse and represent `.dic` files.
//!
//! - Final representation is [`DicFile`]
//! - Parsing logic is implemented in [`DicParser`], entrupoint is [`DicParser::parse`]

use crate::{
	aff::{self, parse_data_field, parse_flags, Flags},
	dictionary::InitializeError,
};
use nom::{
	branch::alt,
	bytes::complete::{is_not, tag},
	character::complete::{newline, space0, space1, u64 as u64_p},
	multi::{many1, separated_list1},
	sequence::tuple,
	IResult, Parser,
};
use nom_supreme::ParserExt;
use std::{
	collections::HashMap,
	fmt::{self, Debug},
	fs::File,
	io::Read,
	path::Path,
};

/// A `.dic` file.
/// Holds every stems and an index to them
#[derive(Debug, Clone)]
pub(crate) struct DicFile {
	/// Stems found in `.dic` file
	stems: Vec<Stem>,
	/// Index to quickly find stem information based on a stem
	index: HashMap<String, Vec<Stem>>,
}

/// Constructors
impl DicFile {
	/// Initialize a [`DicFile`] from raw content
	pub(crate) fn new(content: &str, options: &aff::Options) -> Result<Self, InitializeError> {
		DicParser { options }.parse(content)
	}

	/// Initialize a [`DicFile`] from a `.dic` file
	pub(crate) fn file(path: &Path, options: &aff::Options) -> Result<Self, InitializeError> {
		let mut file = File::open(path)?;
		let mut buffer = String::new();
		file.read_to_string(&mut buffer)?;
		Self::new(&buffer, options)
	}
}

impl DicFile {
	/// Returns every stem registered in a dictionary and its metadata
	pub(crate) fn homonyms<'a>(&'a self, stem: &'a str) -> impl Iterator<Item = &Stem> + 'a {
		self.index.get(stem).into_iter().flatten()
	}
}

/// Parses a [`DicFile`]
struct DicParser<'op> {
	/// Options from an `.aff` file to correctly parse `.dic` file
	options: &'op aff::Options,
}

impl<'op> DicParser<'op> {
	/// Entrypoint of [`DicParser`]
	fn parse(self, i: &str) -> Result<DicFile, InitializeError> {
		let parser_err = |e: nom::Err<nom::error::Error<_>>| InitializeError::Parser(e.to_string());

		let (i, _nb_of_lines) = u64_p.terminated(newline).parse(i).map_err(parser_err)?;
		let (_, stems) = many1(|i| self.parse_entry(i))
			.all_consuming()
			.parse(i)
			.map_err(parser_err)?;

		let mut index = HashMap::default();
		for s in &stems {
			let entry: &mut Vec<Stem> = index.entry(s.root.clone()).or_default();
			entry.push(s.clone());
		}

		Ok(DicFile { stems, index })
	}

	/// Parse a single stem line
	fn parse_entry<'a>(&'a self, i: &'a str) -> IResult<&'a str, Stem> {
		let (i, (root, flags, data_fields)) = tuple((
			// TODO: doesn't take into account escaped slashes
			is_not(" /\n").map(ToOwned::to_owned),
			tag("/")
				.precedes(parse_flags(self.options))
				.opt()
				.map(Option::unwrap_or_default),
			Self::parse_data_fields_with_compression(self.options)
				.preceded_by(space1)
				.opt()
				.map(Option::unwrap_or_default),
		))
		.terminated(space0)
		.terminated(newline)
		.parse(i)?;

		let stem = Stem {
			case: Casing::guess(&root),

			root,
			flags: Flags::new(flags),
			data_fields,
		};

		Ok((i, stem))
	}

	fn parse_data_fields_with_compression<'a>(
		options: &'a aff::Options,
	) -> impl Fn(&'a str) -> IResult<&'a str, Vec<DataField>> + 'a {
		move |i: &str| {
			separated_list1(
				space1,
				alt((
					u64_p.map(|i| {
						options
							.morphological_aliases
							.get(i as usize - 1)
							.cloned()
							.unwrap()
					}),
					parse_data_field.map(|df| vec![df]),
				)),
			)
			.map(|dfl| dfl.into_iter().flatten().collect())
			.parse(i)
		}
	}
}

impl fmt::Display for DicFile {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		writeln!(f, "{}", self.stems.len())?;
		for stem in &self.stems {
			writeln!(f, "{stem}")?;
		}
		Ok(())
	}
}

// TODO: smolstr? smolvec?
/// A single stem with its metadata
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Stem {
	/// Word root
	root: String,
	/// Associated flags (affix and options flags)
	// TODO: link to certain options e.g. `FORBIDDENFLAG`
	pub(crate) flags: Flags,
	/// Associated metadata
	data_fields: Vec<DataField>,
	// alt_spelling
	/// Word root casing
	case: Casing,
}

impl fmt::Display for Stem {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.root)?;
		if !self.flags.has_some() {
			write!(f, "/{}", self.flags)?;
		}
		if !self.data_fields.is_empty() {
			for data in &self.data_fields {
				write!(f, " {data}")?;
			}
		};
		Ok(())
	}
}

/// Casing of a word
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Casing {
	/// All lowercase (“foo”)
	No,
	/// Titlecase, only initial letter is capitalized (“Foo”)
	Init,
	/// All uppercase (“FOO”)
	All,
	/// Mixed capitalization (“fooBar”)
	///
	/// `HUH`
	Huh,
	/// Mixed capitalization, first letter is capitalized (“FooBar”)
	///
	/// `HUHINIT`
	HuhInit,
}

impl fmt::Display for Casing {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::No => write!(f, "No"),
			Self::Init => write!(f, "Init"),
			Self::All => write!(f, "All"),
			Self::Huh => write!(f, "Huh"),
			Self::HuhInit => write!(f, "HuhInit"),
		}
	}
}

impl Casing {
	/// Based on input, guess with a simple algorithm the casing
	pub(crate) fn guess(s: &str) -> Self {
		let first_char_is_upper = s.chars().next().is_some_and(char::is_uppercase);
		let chars = s.chars().skip(1).filter(|c| char::is_alphabetic(*c));

		#[allow(clippy::match_bool)]
		match first_char_is_upper {
			false if chars.clone().all(char::is_lowercase) => Self::No,
			true if chars.clone().all(char::is_lowercase) => Self::Init,
			true if chars.clone().all(char::is_uppercase) => Self::All,
			false => Self::Huh,
			true => Self::HuhInit,
		}
	}

	/// Produces every possible casing variant based on input casing
	pub(crate) fn variants(&self, word: String) -> Vec<String> {
		match self {
			Self::No | Self::Huh => vec![word],
			Self::Init => vec![word.to_lowercase(), word],
			Self::All => vec![Self::capitalize(&word), word.to_lowercase(), word],
			Self::HuhInit => vec![Self::lower_first(&word), word],
		}
	}

	/// Makes the first letter uppercase then the others lowercase
	fn capitalize(word: &str) -> String {
		let mut c = word.chars();
		let mut s = c
			.next()
			.map_or_else(String::new, |f| f.to_uppercase().collect::<String>());
		s.push_str(&c.as_str().to_lowercase());
		s
	}

	/// Makes the first character lowercase
	fn lower_first(word: &str) -> String {
		let mut c = word.chars();
		c.next().map_or_else(String::new, |f| {
			f.to_lowercase().collect::<String>() + c.as_str()
		})
	}
}

// TODO: use &str
/// A single metadata associated to a [`Stem`]
///
/// Each enum member defines which is the identifier of the data field
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum DataField {
	/// `ph`: common misspellings of a word to provide better suggestions.
	///
	/// Hunspell manual: ...
	///
	/// Note: `ph` fields are then inserted in the [`aff::Options::replacements`] table.
	Alternative(String),
	/// `st`: ?
	Stem(String),
	// TODO: check
	/// `al`: used in the `metaphone` part of the suggest workflow?
	///
	/// Hunspell manual: A dictionarwordy item is the stem of its allomorphs.
	/// Morphological generation needs stem, allomorph and affix fields.
	///
	/// Note: rare apparitions in dictionaries
	Allomorph(String),
	/// `po`: hints the nature of a word in a sentence (e.g. noun, verb). Used to analyze words.
	PartOfSpeech(String),

	/// `ds`: ?
	DerivationalSuffix(String),
	/// `is`: ?
	InflectionalSuffix(String),
	/// `ts`: ?
	TerminalSuffix(String),

	/// \[RESERVED] `dp`: ?
	DerivationalPrefix(String),
	/// \[RESERVED] `ip`: ?
	InflectionalPrefix(String),
	/// \[RESERVED] `tp`: ?
	TerminalPrefix(String),

	/// `sp`: ?
	SurfacePrefix(String),
	/// `pa`: ?
	PartsOfCompound(String),
}

impl fmt::Display for DataField {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		// TODO: rename vars when I'll understand what then mean
		match self {
			Self::Alternative(alter) => write!(f, "ph:{alter}"),
			Self::Stem(stem) => write!(f, "st:{stem}"),
			Self::Allomorph(al) => write!(f, "al:{al}"),
			Self::PartOfSpeech(po) => write!(f, "po:{po}"),

			Self::DerivationalSuffix(ds) => write!(f, "ds:{ds}"),
			Self::InflectionalSuffix(is) => write!(f, "is:{is}"),
			Self::TerminalSuffix(ts) => write!(f, "ts:{ts}"),

			Self::DerivationalPrefix(dp) => write!(f, "dp:{dp}"),
			Self::InflectionalPrefix(ip) => write!(f, "ip:{ip}"),
			Self::TerminalPrefix(tp) => write!(f, "tp:{tp}"),

			Self::SurfacePrefix(sp) => write!(f, "sp:{sp}"),
			Self::PartsOfCompound(pa) => write!(f, "pa:{pa}"),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::aff::Flag;

	#[test]
	#[allow(clippy::unnecessary_wraps)]
	fn parse_entry() -> Result<(), nom::Err<nom::error::Error<&'static str>>> {
		use DataField::*;
		macro_rules! test {
			($source:literal -> $res:expr) => {{
				let dic = DicParser {
					options: &aff::Options::default(),
				};
				let (i, s) = dic.parse_entry($source).unwrap();
				assert_eq!(s, $res);
				assert_eq!(i, "");
			}};
		}

		let test_word = String::from("hello");

		test!("word/FGS ph:hello\n" -> super::Stem {
			root: "word".into(),
			flags: Flags::new(vec![Flag::Short('F'), Flag::Short('G'), Flag::Short('S')]),
			data_fields: vec![Alternative(test_word)],
			case: Casing::No,
		});

		Ok(())
	}
}
