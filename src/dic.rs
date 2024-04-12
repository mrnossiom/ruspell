use crate::{
	aff::{self, parse_flag, Flag, FlagType},
	dictionary::InitializeError,
};
use core::fmt;
use nom::{
	bytes::complete::{is_not, tag, take},
	character::complete::{newline, none_of, space1, u64 as u64_p},
	combinator::{fail, map},
	multi::{many0, many1, separated_list1},
	IResult, Parser,
};
use nom_supreme::ParserExt;
use std::{fmt::Debug, fs::File, io::Read, path::Path};

#[derive(Debug)]
pub(crate) struct DicFile {
	stems: Vec<Stem>,
}

impl DicFile {
	pub(crate) fn new(content: &str, options: &aff::Options) -> Result<Self, InitializeError> {
		let parser_err = |e: nom::Err<nom::error::Error<_>>| InitializeError::Parser(e.to_string());

		let (i, _nb_of_lines) = u64_p
			.terminated(newline)
			.parse(content)
			.map_err(parser_err)?;
		let (_, stems) = many0(parse_entry(&options.flag_ty))
			.all_consuming()
			.parse(i)
			.map_err(parser_err)?;

		Ok(Self { stems })
	}

	pub(crate) fn file(path: &Path, options: &aff::Options) -> Result<Self, InitializeError> {
		let mut file = File::open(path)?;
		let mut buffer = String::new();
		file.read_to_string(&mut buffer)?;
		Self::new(&buffer, options)
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
#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Stem {
	stem: String,
	flags: Vec<Flag>,
	data: Vec<DataField>,
	// alt_spelling
	case: Casing,
}

impl fmt::Display for Stem {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.stem)?;
		if !self.flags.is_empty() {
			write!(f, "/")?;
			for flag in &self.flags {
				write!(f, "{flag}")?;
			}
		}
		if !self.data.is_empty() {
			for data in &self.data {
				write!(f, " {data}")?;
			}
		};
		Ok(())
	}
}

fn parse_flags(fty: &FlagType) -> impl Fn(&str) -> IResult<&str, Vec<Flag>> + '_ {
	move |i: &str| match fty {
		FlagType::Short | FlagType::Long | FlagType::Utf8 => many1(parse_flag(fty))(i),
		FlagType::Numeric => separated_list1(tag(","), parse_flag(fty))(i),
	}
}

fn letter(i: &str) -> IResult<&str, char> {
	let t = i.chars().next().ok_or_else(|| {
		fail::<&str, char, nom::error::Error<&str>>(i).expect_err("just constructed an error")
	})?;
	if t == '\\' {
		Ok((
			&i[2..],
			i.chars().nth(1).ok_or_else(|| {
				fail::<&str, char, nom::error::Error<&str>>(i)
					.expect_err("just constructed an error")
			})?,
		))
	} else {
		none_of("\"")(i)
	}
}

fn parse_stem_and_flags(
	fty: &FlagType,
) -> impl Fn(&str) -> IResult<&str, (String, Vec<Flag>)> + '_ {
	move |i: &str| {
		// TODO: doesn't take into account escaped slashes
		let (i, stem) = is_not(" /\n").parse(i)?;
		let (i, flags) = tag("/").precedes(parse_flags(fty)).opt().parse(i)?;

		Ok((i, (stem.to_owned(), flags.unwrap_or_default())))
	}
}

fn parse_data_fields(i: &str) -> IResult<&str, Vec<DataField>> {
	many0(space1.precedes(DataField::parse))(i)
}

fn parse_entry(fty: &FlagType) -> impl Fn(&str) -> IResult<&str, Stem> + '_ {
	move |i| {
		let (i, (stem, flags)) = parse_stem_and_flags(fty)(i)?;
		let (i, data) = parse_data_fields.terminated(newline).parse(i)?;

		let stem = Stem {
			case: Casing::guess(&stem),

			stem,
			flags,
			data,
		};

		Ok((i, stem))
	}
}

#[derive(Debug, PartialEq, Eq)]
enum Casing {
	/// All lowercase (“foo”)
	No,
	/// Titlecase, only initial letter is capitalized (“Foo”)
	Init,
	/// All uppercase (“FOO”)
	All,
	/// Mixed capitalization (“fooBar”)
	Huh,
	/// Mixed capitalization, first letter is capitalized (“FooBar”)
	HuhInit,
}

impl Casing {
	fn guess(s: &str) -> Self {
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
	PartsOfCoumpound(String),
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
			"pa" => map(till_space, Self::PartsOfCoumpound)(i),

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
			Self::PartsOfCoumpound(pa) => write!(f, "pa:{pa}"),
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
		test!("pa:hello" => PartsOfCoumpound(TEST_WORD.into()));

		Ok(())
	}

	#[test]
	fn can_parse_data_field_list() -> Result<(), nom::Err<nom::error::Error<&'static str>>> {
		use DataField::*;
		macro_rules! test {
			($source:literal => $res:expr) => {{
				let (i, s) = parse_data_fields($source)?;
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
	fn can_parse_entry() -> Result<(), nom::Err<nom::error::Error<&'static str>>> {
		use DataField::*;
		macro_rules! test {
			($source:literal -> $res:expr) => {{
				let (i, s) = parse_entry(&FlagType::Short)($source)?;
				assert_eq!(s, $res);
				assert_eq!(i, "");
			}};
		}

		test!("word/FGS ph:hello\n" -> super::Stem {
			stem: "word".into(),
			flags: vec![Flag::Short('F'), Flag::Short('G'), Flag::Short('S')],
			data: vec![Alternative(TEST_WORD.into())],
			case: Casing::No,
		});

		Ok(())
	}
}
