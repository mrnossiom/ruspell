//! Logic to parse and represent `.dic` files.
//!
//! - Final represnetation is [`DicFile`]
//! - Parsing logic is implemented in [`DicParser`], entrupoint is [`DicParser::parse`]

use crate::{
	aff::{self, parse_flags, Flag},
	dictionary::InitializeError,
};
use nom::{
	bytes::complete::{is_not, tag, take},
	character::complete::{newline, space1, u64 as u64_p},
	combinator::map,
	multi::many0,
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
		let (_, stems) = many0(|i| self.parse_entry(i))
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
				.precedes(parse_flags(&self.options.flag_ty))
				.opt()
				.map(Option::unwrap_or_default),
			Self::parse_data_fields,
		))
		.terminated(newline)
		.parse(i)?;

		let stem = Stem {
			case: Casing::guess(&root),

			root,
			flags,
			data_fields,
		};

		Ok((i, stem))
	}

	/// Parse stem associated data fields
	fn parse_data_fields(i: &str) -> IResult<&str, Vec<DataField>> {
		many0(space1.precedes(Self::parse_data_field))(i)
	}

	/// Parse a single data field
	fn parse_data_field(i: &str) -> IResult<&str, DataField> {
		let (i, discriminant) = take(2usize).terminated(tag(":")).parse(i)?;

		let till_space = map(is_not(" \n"), ToString::to_string);

		match discriminant {
			"ph" => map(till_space, DataField::Alternative)(i),
			"st" => map(till_space, DataField::Stem)(i),
			"al" => map(till_space, DataField::Allomorph)(i),
			"po" => map(till_space, DataField::PartOfSpeech)(i),

			"ds" => map(till_space, DataField::DerivationalSuffix)(i),
			"is" => map(till_space, DataField::InflectionalSuffix)(i),
			"ts" => map(till_space, DataField::TerminalSuffix)(i),

			"dp" => map(till_space, DataField::DerivationalPrefix)(i),
			"ip" => map(till_space, DataField::InflectionalPrefix)(i),
			"tp" => map(till_space, DataField::TerminalPrefix)(i),

			"sp" => map(till_space, DataField::SurfacePrefix)(i),
			"pa" => map(till_space, DataField::PartsOfCompound)(i),

			_ => todo!("error out with invalid data field type"),
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
	pub(crate) flags: Vec<Flag>,
	/// Associated metadata
	data_fields: Vec<DataField>,
	// alt_spelling
	/// Word root casing
	case: Casing,
}

impl fmt::Display for Stem {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.root)?;
		if !self.flags.is_empty() {
			write!(f, "/")?;
			for flag in &self.flags {
				write!(f, "{flag}")?;
			}
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

impl Casing {
	/// Based on input, guess with a simple algorithm the casing
	pub(crate) fn guess(s: &str) -> Self {
		let first_char_is_upper = s.chars().next().is_some_and(char::is_uppercase);
		let chars = s.chars().skip(1);

		#[allow(clippy::match_bool)]
		match first_char_is_upper {
			false if chars.clone().all(char::is_lowercase) => Self::No,
			true if chars.clone().all(char::is_lowercase) => Self::Init,
			true if chars.clone().all(char::is_uppercase) => Self::All,
			false => Self::HuhInit,
			true => Self::Huh,
		}
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
	/// Hunspell manual: A dictionary item is the stem of its allomorphs.
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

	const TEST_WORD: &str = "hello";

	#[test]
	fn parse_every_data_field_form() -> Result<(), nom::Err<nom::error::Error<&'static str>>> {
		macro_rules! test {
			($source:literal => $res:expr) => {{
				use DataField::*;
				let (i, df) = DicParser::parse_data_field($source)?;
				assert_eq!(df, $res);
				assert_eq!(i, "");
			}};
		}

		test!("ph:hello" => Alternative(TEST_WORD.into()));
		// TODO
		// test_df!("ph:hello->bye" => Alternative(TEST_WORD.into()));

		test!("st:hello" => Stem(TEST_WORD.into()));
		test!("al:hello" => Allomorph(TEST_WORD.into()));
		test!("po:hello" => PartOfSpeech(TEST_WORD.into()));

		test!("ds:hello" => DerivationalSuffix(TEST_WORD.into()));
		test!("is:hello" => InflectionalSuffix(TEST_WORD.into()));
		test!("ts:hello" => TerminalSuffix(TEST_WORD.into()));

		test!("dp:hello" => DerivationalPrefix(TEST_WORD.into()));
		test!("ip:hello" => InflectionalPrefix(TEST_WORD.into()));
		test!("tp:hello" => TerminalPrefix(TEST_WORD.into()));

		test!("sp:hello" => SurfacePrefix(TEST_WORD.into()));
		test!("pa:hello" => PartsOfCompound(TEST_WORD.into()));

		Ok(())
	}

	#[test]
	fn parse_data_field_list() -> Result<(), nom::Err<nom::error::Error<&'static str>>> {
		use DataField::*;
		macro_rules! test {
			($source:literal => $res:expr) => {{
				let (i, s) = DicParser::parse_data_fields($source)?;
				assert_eq!(s.as_slice(), $res.as_slice());
				assert_eq!(i, "");
			}};
		}

		test!(" ph:hello al:hello" => [Alternative(TEST_WORD.into()), Allomorph(TEST_WORD.into())]);
		test!(" ph:hello al:hello po:hello" => [Alternative(TEST_WORD.into()), Allomorph(TEST_WORD.into()), PartOfSpeech(TEST_WORD.into())]);
		// TODO
		// test_df!("ph:hello->bye" => Alternative(hello.clone()));

		Ok(())
	}

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

		test!("word/FGS ph:hello\n" -> super::Stem {
			root: "word".into(),
			flags: vec![Flag::Short('F'), Flag::Short('G'), Flag::Short('S')],
			data_fields: vec![Alternative(TEST_WORD.into())],
			case: Casing::No,
		});

		Ok(())
	}
}
