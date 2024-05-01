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

pub(crate) struct DicFile {
	stems: Vec<Stem>,
	index: HashMap<String, Vec<Stem>>,
}

impl DicFile {
	pub(crate) fn new(content: &str, options: &aff::Options) -> Result<Self, InitializeError> {
		DicParser { options }.parse(content)
	}

	pub(crate) fn file(path: &Path, options: &aff::Options) -> Result<Self, InitializeError> {
		let mut file = File::open(path)?;
		let mut buffer = String::new();
		file.read_to_string(&mut buffer)?;
		Self::new(&buffer, options)
	}

	pub(crate) fn homonyms<'a>(&'a self, stem: &'a str) -> impl Iterator<Item = &Stem> + 'a {
		self.index.get(stem).into_iter().flatten()
	}
}

struct DicParser<'options> {
	options: &'options aff::Options,
}

impl<'options> DicParser<'options> {
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

	fn parse_data_fields(i: &str) -> IResult<&str, Vec<DataField>> {
		many0(space1.precedes(DataField::parse))(i)
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Stem {
	root: String,
	pub(crate) flags: Vec<Flag>,
	data_fields: Vec<DataField>,
	// alt_spelling
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum DataField {
	Alternative(String),
	Stem(String),
	Allomorph(String),
	PartOfSpeech(String),

	DerivationalSuffix(String),
	InflectionalSuffix(String),
	TerminalSuffix(String),

	// There are reserved but not used
	DerivationalPrefix(String),
	InflectionalPrefix(String),
	TerminalPrefix(String),

	SurfacePrefix(String),
	PartsOfCompound(String),
}

impl DataField {
	fn parse(i: &str) -> IResult<&str, Self> {
		let (i, discriminant) = take(2usize).terminated(tag(":")).parse(i)?;

		let till_space = map(is_not(" \n"), ToString::to_string);

		match discriminant {
			"ph" => map(till_space, Self::Alternative)(i),
			"st" => map(till_space, Self::Stem)(i),
			"al" => map(till_space, Self::Allomorph)(i),
			"po" => map(till_space, Self::PartOfSpeech)(i),

			"ds" => map(till_space, Self::DerivationalSuffix)(i),
			"is" => map(till_space, Self::InflectionalSuffix)(i),
			"ts" => map(till_space, Self::TerminalSuffix)(i),

			"dp" => map(till_space, Self::DerivationalPrefix)(i),
			"ip" => map(till_space, Self::InflectionalPrefix)(i),
			"tp" => map(till_space, Self::TerminalPrefix)(i),

			"sp" => map(till_space, Self::SurfacePrefix)(i),
			"pa" => map(till_space, Self::PartsOfCompound)(i),

			_ => todo!("error out with invalid data field type"),
		}
	}
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
	fn can_parse_every_data_field_form() -> Result<(), nom::Err<nom::error::Error<&'static str>>> {
		macro_rules! test {
			($source:literal => $res:expr) => {{
				use DataField::*;
				let (i, df) = DataField::parse($source)?;
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
	fn can_parse_data_field_list() -> Result<(), nom::Err<nom::error::Error<&'static str>>> {
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
	fn can_parse_entry() -> Result<(), nom::Err<nom::error::Error<&'static str>>> {
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
