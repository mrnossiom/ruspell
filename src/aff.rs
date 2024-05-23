//! Logic to parse and represent `.aff` files.
//!
//! - Final representation is [`AffFile`]
//! - Parsing logic is implemented in [`AffParser`], entrupoint is [`AffParser::parse`]

use crate::{dic::DataField, dictionary::InitializeError, trie::Trie};
use nom::{
	branch::alt,
	bytes::complete::{is_not, tag, take, take_while1},
	character::complete::{newline, satisfy, space0, space1, u16 as u16_p, u64 as u64_p},
	combinator::{map, opt},
	multi::{many1, many_m_n, separated_list1},
	sequence::{delimited, preceded, terminated, tuple},
	IResult, Parser,
};
use nom_supreme::ParserExt;
use regex::Regex;
use std::{
	fmt::{self, Debug},
	fs::File,
	io::Read,
	marker::PhantomData,
	path::Path,
	str::FromStr,
};

/// An `.aff` file.
/// Holds defined options, additional flags and affixes with an index each.
pub(crate) struct AffFile {
	/// Additional options
	pub(crate) options: Options,
	/// Flags others than affixes
	pub(crate) additional_flags: AdditionalFlags,

	// TODO: do a self-referencing struct
	/// Holds every prefix
	prefixes: Vec<Affix<Prefix>>,
	/// A trie that maps prefixes.
	///
	/// Simplifies a lot the production of prefix forms during lookup.
	/// See [`Dictionary::produce_prefix_and_cross_forms`](crate::Dictionary::produce_prefix_and_cross_forms).
	pub(crate) prefix_index: Trie<Affix<Prefix>>,
	/// Holds every suffix
	suffixes: Vec<Affix<Suffix>>,
	/// A trie that maps reversed suffixes
	pub(crate) suffix_index: Trie<Affix<Suffix>>,
}

// pub type IResult<I, O, E = ParserError<I>> = Result<(I, O), nom::Err<E>>;

// #[derive(Debug, thiserror::Error)]
// enum ParserError<T> {
// 	Parser(nom::error::Error<T>),
// }

impl AffFile {
	/// Initializes a new [`AffFile`] from raw content
	pub(crate) fn new(content: &str) -> Result<Self, InitializeError> {
		let AffParser {
			options,
			prefixes,
			suffixes,
			additional_flags,
		} = AffParser::default().parse(content)?;

		// Computing tries

		let mut prefix_index = Trie::default();
		prefixes
			.iter()
			// .filter_map(|prefix| prefix.affix.as_ref())
			.cloned()
			.for_each(|prefix| prefix_index.insert(&prefix.add.clone(), prefix));

		let mut suffix_index = Trie::default();
		suffixes
			.iter()
			// .filter_map(|suffix| suffix.affix.as_ref())
			// You need to reverse suffixes
			// .map(|suffix| suffix.chars().rev().collect::<String>())
			.cloned()
			.for_each(|suffix| {
				suffix_index.insert(&suffix.add.chars().rev().collect::<String>(), suffix);
			});

		Ok(Self {
			options,
			additional_flags,
			prefixes,
			prefix_index,
			suffixes,
			suffix_index,
		})
	}

	/// Initializes a new [`AffFile`] from a file
	pub(crate) fn file(path: &Path) -> Result<Self, InitializeError> {
		let mut file = File::open(path)?;
		let mut buffer = String::new();
		file.read_to_string(&mut buffer)?;
		Self::new(&buffer)
	}
}

#[allow(clippy::struct_excessive_bools)]
#[derive(Debug, Default)]
/// Additional options defined in `.aff` file
pub(crate) struct Options {
	/// `SET`
	encoding: Encoding,
	/// `FLAG`
	// TODO: check pub
	pub(crate) flag_ty: FlagType,
	/// `COMPLEXPREFIXES`
	complex_prefixes: bool,
	/// `LANG`
	lang: Option<Lang>,
	/// `IGNORE`
	pub(crate) ignore: IgnoreList,
	/// `AF`
	/// Flags can be compressed and replaced with an ordinal number.
	/// Table is `0`-indexed.
	pub(crate) flag_aliases: Vec<Vec<Flag>>,
	/// `AM`
	/// Table is `0`-indexed.
	pub(crate) morphological_aliases: Vec<Vec<DataField>>,

	// ——— for suggestions
	/// `KEY`
	key: Vec<Vec<char>>,
	/// `TRY`
	try_chars: Vec<char>,
	/// `MAXCPDSUGS`
	max_compound_suggestions: u64,
	/// `MAXNGRAMSUGS`
	max_ngram_suggestions: u64,
	/// `MAXDIFF`
	/// Value is 1-10
	max_diff: u64,
	/// `ONLYMAXDIFF`
	only_max_diff: bool,
	/// `NOSPLITSUGS`
	no_split_suggestions: bool,
	/// `SUGSWITHDOTS`
	suggest_with_dots: bool,
	/// `REP`
	replacements: Vec<Replacement>,
	/// `MAP`
	maps: Vec<Replacement>,
	/// `PHONE`
	phonetic_replacements: Vec<Replacement>,
	/// `FORBIDWARN`
	forbid_warn: bool,

	// ——— for compounding
	/// `BREAK`
	pub(crate) compound_split_points: Vec<String>,
	/// `COMPOUNDRULE`
	compound_rules: Vec<String>,
	/// `COMPOUNDMIN`
	compound_min_parts_length: u64,
	/// `COMPOUNDMORESUFFIXES`
	compound_allow_mul_suffixes: bool,
	/// `COMPOUNDWORDMAX`
	compound_max_word: u64,
	/// `CHECKCOMPOUNDDUP`
	compound_check_duplication: bool,
	/// `CHECKCOMPOUNDREP`
	compound_check_replacement: bool,
	/// `CHECKCOMPOUNDCASE`
	compound_check_case: bool,
	/// `CHECKCOMPOUNDTRIPLE`
	compound_check_triple_repeating_letters: bool,
	/// `SIMPLIFIEDTRIPLE`
	// TODO: find a good explicit name
	compound_simplify_previous_rule: bool,
	/// `CHECKCOMPOUNDPATTERN`
	compound_check_patterns: Vec<Pattern>,
	/// `COMPOUNDSYLLABLE`
	// TODO: rework
	compound_syllable: (u64, Vec<char>),

	// ——— other
	/// `FULLSTRIP`
	full_strip: bool,
	/// `ICONV`
	pub(crate) input_conversion: ConversionTable,
	/// `OCONV`
	pub(crate) output_conversion: ConversionTable,
	/// `WORDCHARS`
	word_chars: Vec<char>,
	/// `CHECKSHARPS`
	check_sharps: bool,
}

#[derive(Debug, Default)]
/// Flags that are not affixes but define additional behaviour
pub(crate) struct AdditionalFlags {
	// ——— for suggestions
	/// `NOSUGGEST`
	no_suggest: Option<Flag>,
	/// `WARN`
	warn: Option<Flag>,

	// ——— for compounding
	/// `COMPOUNDFLAG`
	compound: Option<Flag>,
	/// `COMPOUNDBEGIN`
	compound_begin: Option<Flag>,
	/// `COMPOUNDMIDDLE`
	compound_middle: Option<Flag>,
	/// `COMPOUNDLAST`
	compound_last: Option<Flag>,
	/// `ONLYINCOMPOUND`
	compound_only: Option<Flag>,
	/// `COMPOUNDPERMITFLAG`
	compound_permit_affix: Option<Flag>,
	/// `COMPOUNDFORBIDFLAG`
	compound_forbid_affix: Option<Flag>,
	/// `COMPOUNDROOT`
	compound_root: Option<Flag>,
	/// `FORCEUCASE`
	compound_force_case: Option<Flag>,
	/// `SYLLABLENUM`
	syllabes: Option<Vec<Flag>>,

	// ——— other
	/// `CIRCUMFIX`
	circum_fix: Option<Flag>,
	/// `FORBIDDENWORD`
	pub(crate) forbidden_word: Option<Flag>,
	/// `KEEPCASE`
	pub(crate) keep_case: Option<Flag>,
	/// `LEMMA_PRESENT`
	#[deprecated]
	lemma_present: Option<Flag>,
	/// `NEEDAFFIX`, `PSEUDOROOT`
	/// Can't exist on it's own
	need_affix: Option<Flag>,
	/// `SUBSTANDARD`
	sub_standard: Option<Flag>,
	/// `WORDCHARS`
	word_chars: Option<Flag>,
}

/// A lang identifier (e.g. `fr_FR`, `en_US`)
#[derive(Debug)]
pub(crate) struct Lang(String);

#[derive(Debug)]
pub(crate) struct Pattern;

/// Encoding specified in `.aff` file
#[derive(Debug, Default)]
enum Encoding {
	/// `UTF-8`
	#[default]
	Utf8,
	/// `ISO8859-{i}` where `i` can be 1-10, 13-15
	Iso8859(u16),
	/// `KOI8-R`
	Koi8R,
	/// `KOI8-U`
	Koi8U,
	/// `cp1251`
	Cp1251,
	/// `ISCII-DEVANAGARI`
	IsciiDevanagari,
}

impl FromStr for Encoding {
	type Err = ();

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		match s {
			"UTF-8" => Ok(Self::Utf8),
			"ISO8859-1" => Ok(Self::Iso8859(1)),
			"ISO8859-2" => Ok(Self::Iso8859(2)),
			"ISO8859-3" => Ok(Self::Iso8859(3)),
			"ISO8859-4" => Ok(Self::Iso8859(4)),
			"ISO8859-5" => Ok(Self::Iso8859(5)),
			"ISO8859-6" => Ok(Self::Iso8859(6)),
			"ISO8859-7" => Ok(Self::Iso8859(7)),
			"ISO8859-8" => Ok(Self::Iso8859(8)),
			"ISO8859-9" => Ok(Self::Iso8859(9)),
			"ISO8859-10" => Ok(Self::Iso8859(10)),
			"ISO8859-13" => Ok(Self::Iso8859(13)),
			"ISO8859-14" => Ok(Self::Iso8859(14)),
			"ISO8859-15" => Ok(Self::Iso8859(15)),
			"KOI8-R" => Ok(Self::Koi8R),
			"KOI8-U" => Ok(Self::Koi8U),
			"cp1251" => Ok(Self::Cp1251),
			"ISCII-DEVANAGARI" => Ok(Self::IsciiDevanagari),

			_ => Err(()),
		}
	}
}

/// Is only used to define  [`Affix`]
#[derive(Debug, Clone)]
pub(crate) struct Prefix;
/// Is only used to define [`Affix`]
#[derive(Debug, Clone)]
pub(crate) struct Suffix;

/// Represent a flag affix, it could be either a prefix (`PFX`) of a suffix (`SFX`).
/// It works the same for both so `AFX` is used in the following example.
///
/// ```aff
/// AFX A Y 1
/// AFX A   0     re      .
/// #   ^fg ^strp ^add    ^cond
/// ````
// TODO: they have flags too?
#[derive(Debug, Clone)]
pub(crate) struct Affix<T> {
	/// Flag that identifies this affix in `.dic` files
	pub(crate) flag: Flag,
	/// Does this affix supports cross product (being associated with other affixes)
	pub(crate) cross_product: bool,

	/// Affix part added to stem
	pub(crate) add: String,
	/// What to strip to stem before adding affix part
	pub(crate) strip: String,
	/// Stem must meet condition before affix is applied
	pub(crate) condition: Option<Regex>,

	/// Associated flags
	pub(crate) flags: Flags,

	/// Associated metadata to further enhance suggestion and lookup
	pub(crate) data_fields: Vec<DataField>,

	/// `T` is either [`Prefix`] or [`Suffix`]. Specializes the affix, though
	/// they share the same structure.
	kind: PhantomData<T>,
}

impl fmt::Display for Affix<Prefix> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "({}-, ", self.add,)?;
		if let Some(condition) = &self.condition {
			write!(f, "on {condition}, ")?;
		}
		write!(
			f,
			"{}{}",
			self.flag,
			if self.cross_product { "×" } else { "" }
		)?;
		if !self.strip.is_empty() {
			write!(f, ", -{}", self.strip)?;
		}
		if self.flags.has_some() {
			write!(f, ", with {}", self.flags)?;
		}
		write!(f, ")")
	}
}

impl fmt::Display for Affix<Suffix> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "(-{}, ", self.add,)?;
		if let Some(condition) = &self.condition {
			write!(f, "on {condition}, ")?;
		}
		write!(
			f,
			"{}{}",
			self.flag,
			if self.cross_product { "×" } else { "" }
		)?;
		if !self.strip.is_empty() {
			write!(f, ", -{}", self.strip)?;
		}
		if self.flags.has_some() {
			write!(f, ", with {:#}", self.flags)?;
		}
		write!(f, ")")
	}
}

#[derive(Debug)]
pub(crate) struct Replacement {
	strip: String,
	add: String,
}

/// Used for input and output conversion tables (`OCONV`, `ICONV`) which normalizes
/// some characters for lookup.
#[derive(Debug, Default)]
pub(crate) struct ConversionTable {
	/// Inner normalize table
	replacements: Vec<(Regex, String)>,
}

impl ConversionTable {
	/// Used to construct a [`ConversionTable`]
	fn add(&mut self, pattern: &str, rep: &str) {
		let mut boundairies = (false, false);

		let pattern = pattern.strip_prefix('_').map_or(pattern, |pat| {
			boundairies.0 = true;
			pat
		});

		let pattern = pattern.strip_suffix('_').map_or(pattern, |pat| {
			boundairies.1 = true;
			pat
		});

		let pattern = format!(
			"{}{pattern}{}",
			if boundairies.0 { "^" } else { "" },
			if boundairies.1 { "$" } else { "" }
		);

		let pat = Regex::new(&pattern).unwrap();

		// Hunspell defines underscores as spaces in replacement
		let rep = rep.replace('_', " ");

		self.replacements.push((pat, rep));
	}

	/// Normalizes the string
	pub(crate) fn convert(&self, word: &mut String) {
		if self.replacements.is_empty() {
			return;
		}

		// TODO

		// for i in 0..word.len() {
		// 	let mut repl: Vec<_> = self
		// 		.replacements
		// 		.iter()
		// 		.map(|(r, rep)| r.replace(&word, rep))
		// 		.collect();

		// 	repl.sort_by(|news, news2| news.len().cmp(&news2.len()));

		// 	let repl = match repl.into_iter().next() {
		// 		Some(m) => word = m,
		// 		None => continue,
		// 	};
		// }
	}
}

/// Parses an `.aff` file
#[derive(Default)]
struct AffParser {
	/// Parsed options
	pub(crate) options: Options,
	/// Parsed additional flags
	pub(crate) additional_flags: AdditionalFlags,

	/// Every parsed prefix
	pub(crate) prefixes: Vec<Affix<Prefix>>,
	/// Every parsed suffix
	pub(crate) suffixes: Vec<Affix<Suffix>>,
}

impl AffParser {
	/// Entrypoint to parse an `.aff` file into a [`AffFile`]
	fn parse(mut self, content: &str) -> Result<Self, InitializeError> {
		many1(alt((
			Self::parse_directive(&mut self),
			tag("#")
				.terminated(is_not("\n"))
				.terminated(newline)
				.value(()),
			newline.value(()),
		)))
		.all_consuming()
		.parse(content)
		.map_err(|e: nom::Err<_>| InitializeError::Parser(e.to_string()))?;

		Ok(self)
	}

	#[allow(clippy::too_many_lines)]
	/// Takes care of parsing a whole line with the ending newline
	fn parse_directive<'a>(&mut self) -> impl FnMut(&'a str) -> IResult<&'a str, ()> + '_ {
		let Self {
			prefixes,
			suffixes,
			additional_flags,
			options,
		} = self;

		move |i: &'a str| {
			let is_directive_char = |c: char| matches!(c, 'A'..='Z' | '_');
			let (i, directive_name) = terminated(take_while1(is_directive_char), space0)(i)?;

			let mut str_till_end = terminated(is_not("\n"), newline);
			let mut chars_till_end = map(terminated(is_not("\n"), newline), |s: &str| {
				s.chars().collect()
			});

			let res = match directive_name {
				"SET" => {
					let (i, encoding) = str_till_end(i)?;
					(i, options.encoding = Encoding::from_str(encoding).unwrap())
				}
				"FLAG" => {
					let (i, flag_ty) = str_till_end(i)?;
					(i, options.flag_ty = FlagType::from_str(flag_ty).unwrap())
				}
				"COMPLEXPREFIXES" => (i, options.complex_prefixes = true),
				"LANG" => {
					let (i, lang) = str_till_end(i)?;
					(i, options.lang = Some(Lang(lang.to_owned())))
				}
				"IGNORE" => {
					let (i, chars) = chars_till_end(i)?;
					(i, options.ignore = IgnoreList(chars))
				}
				"AF" => {
					let (i, num) = u64_p.terminated(newline).parse(i)?;
					let (i, reps) = many_m_n(
						usize::try_from(num).unwrap(),
						usize::try_from(num).unwrap(),
						tag("AF ")
							.precedes(Self::parse_flags(options))
							.terminated(space0)
							.terminated(newline),
					)(i)?;

					for (i, rep) in reps.iter().enumerate() {
						log::debug!("(AF) aliased {} to {rep:?}", i + 1);
					}

					(i, options.flag_aliases = reps)
				}
				"AM" => {
					let (i, num) = u64_p.terminated(newline).parse(i)?;
					let (i, reps) = many_m_n(
						usize::try_from(num).unwrap(),
						usize::try_from(num).unwrap(),
						tag("AM ")
							.precedes(Self::parse_data_fields)
							.terminated(space0)
							.terminated(newline),
					)(i)?;

					for (i, rep) in reps.iter().enumerate() {
						log::debug!("(AM) aliased {} to {rep:?}", i + 1);
					}

					(i, options.morphological_aliases = reps)
				}

				// ——— for suggestions
				"KEY" => {
					let (i, lang) = str_till_end(i)?;
					let chars_split_by_pipe =
						lang.split('|').map(|s| s.chars().collect()).collect();
					(i, options.key = chars_split_by_pipe)
				}
				"TRY" => {
					let (i, chars) = chars_till_end(i)?;
					(i, options.try_chars = chars)
				}

				"NOSUGGEST" => {
					let (i, flag) = Self::parse_flag(&options.flag_ty)(i)?;
					(i, set_flag(&mut additional_flags.no_suggest, flag))
				}
				"MAXCPDSUGS" => {
					let (i, num) = u64_p(i)?;
					(i, options.max_compound_suggestions = num)
				}
				"MAXNGRAMSUGS" => {
					let (i, num) = u64_p(i)?;
					(i, options.max_ngram_suggestions = num)
				}
				"MAXDIFF" => {
					let (i, num) = u64_p(i)?;
					// TODO: make a parser error instead of an assert
					assert!((1..=10).contains(&num));
					(i, options.max_diff = num)
				}
				"ONLYMAXDIFF" => (i, options.only_max_diff = true),
				"NOSPLITSUGS" => (i, options.no_split_suggestions = true),
				"SUGSWITHDOTS" => (i, options.suggest_with_dots = true),
				"REP" => {
					let (i, num) = u64_p.terminated(newline).parse(i)?;
					let (i, mut reps) = many_m_n(
						usize::try_from(num).unwrap(),
						usize::try_from(num).unwrap(),
						tag("REP ")
							.precedes(tuple((is_not(" ").terminated(space1), is_not("\n"))).map(
								|(s, rep): (&str, &str)| Replacement {
									strip: s.to_string(),
									add: rep.to_string(),
								},
							))
							.terminated(space0)
							.terminated(newline),
					)(i)?;

					(i, options.replacements.append(&mut reps))
				}
				"MAP" => {
					options.maps = todo!("MAP");
				}
				"PHONE" => {
					options.phonetic_replacements = todo!("PHONE");
				}
				"WARN" => {
					let (i, flag) = Self::parse_flag(&options.flag_ty)(i)?;
					(i, set_flag(&mut additional_flags.warn, flag))
				}
				"FORBIDWARN" => (i, options.forbid_warn = true),

				// ——— for compounding
				"BREAK" => {
					options.compound_split_points = todo!("BREAK");
				}
				"COMPOUNDRULE" => {
					let (i, num) = u64_p.terminated(newline).parse(i)?;
					let (i, mut rules) = many_m_n(
						usize::try_from(num).unwrap(),
						usize::try_from(num).unwrap(),
						|i: &'a str| -> IResult<&'a str, String> {
							let (i, pattern) =
								delimited(tag("COMPOUNDRULE "), is_not("\n"), newline)(i)?;

							Ok((i, pattern.to_string()))
						},
					)(i)?;

					// TODO: maybe warn is more than one block were in place?
					(i, options.compound_rules.append(&mut rules))
				}
				"COMPOUNDMIN" => {
					let (i, num) = u64_p(i)?;
					(i, options.compound_min_parts_length = num)
				}
				"COMPOUNDFLAG" => {
					let (i, flag) = Self::parse_flag(&options.flag_ty)(i)?;
					(i, set_flag(&mut additional_flags.compound, flag))
				}
				"COMPOUNDBEGIN" => {
					let (i, flag) = Self::parse_flag(&options.flag_ty)(i)?;
					(i, set_flag(&mut additional_flags.compound_begin, flag))
				}
				"COMPOUNDLAST" => {
					let (i, flag) = Self::parse_flag(&options.flag_ty)(i)?;
					(i, set_flag(&mut additional_flags.compound_last, flag))
				}
				"COMPOUNDMIDDLE" => {
					let (i, flag) = Self::parse_flag(&options.flag_ty)(i)?;
					(i, set_flag(&mut additional_flags.compound_middle, flag))
				}
				"ONLYINCOMPOUND" => {
					let (i, flag) = Self::parse_flag(&options.flag_ty)(i)?;
					(i, set_flag(&mut additional_flags.compound_only, flag))
				}
				"COMPOUNDPERMITFLAG" => {
					let (i, flag) = Self::parse_flag(&options.flag_ty)(i)?;
					(
						i,
						set_flag(&mut additional_flags.compound_permit_affix, flag),
					)
				}
				"COMPOUNDFORBIDFLAG" => {
					let (i, flag) = Self::parse_flag(&options.flag_ty)(i)?;
					(
						i,
						set_flag(&mut additional_flags.compound_forbid_affix, flag),
					)
				}
				"COMPOUNDMORESUFFIXES" => (i, options.compound_allow_mul_suffixes = true),
				"COMPOUNDROOT" => {
					let (i, flag) = Self::parse_flag(&options.flag_ty)(i)?;
					(i, set_flag(&mut additional_flags.compound_root, flag))
				}
				"COMPOUNDWORDMAX" => {
					let (i, num) = u64_p(i)?;
					(i, options.compound_max_word = num)
				}
				"CHECKCOMPOUNDDUP" => (i, options.compound_check_duplication = true),
				"CHECKCOMPOUNDREP" => (i, options.compound_check_replacement = true),
				"CHECKCOMPOUNDCASE" => (i, options.compound_check_case = true),
				"CHECKCOMPOUNDTRIPLE" => {
					(i, options.compound_check_triple_repeating_letters = true)
				}
				"SIMPLIFIEDTRIPLE" => (i, options.compound_simplify_previous_rule = true),
				"CHECKCOMPOUNDPATTERN" => {
					options.compound_check_patterns = todo!("CHECKCOMPOUNDPATTERN");
				}
				"FORCEUCASE" => {
					let (i, flag) = Self::parse_flag(&options.flag_ty)(i)?;
					(i, set_flag(&mut additional_flags.compound_force_case, flag))
				}
				"COMPOUNDSYLLABLE" => {
					let (i, flag) = terminated(u64_p, tag(" "))(i)?;
					let (i, vowels) = chars_till_end(i)?;
					(i, options.compound_syllable = (flag, vowels))
				}
				"SYLLABLENUM" => {
					let (i, flag) =
						separated_list1(tag(","), Self::parse_flag(&options.flag_ty))(i)?;
					(i, set_flag(&mut additional_flags.syllabes, flag))
				}

				// ——— for affix creation
				"PFX" => {
					// One flag mean multiple affixes
					let (i, (flag, cross_product, num)) = tuple((
						Self::parse_flag(&options.flag_ty),
						map(take(1_usize).preceded_by(space1), |s| {
							Self::parse_cross_product(s).unwrap()
						}),
						u64_p.preceded_by(space1).terminated(newline),
					))(i)?;
					let (i, mut pfxs) = many_m_n(
						usize::try_from(num).unwrap(),
						usize::try_from(num).unwrap(),
						tag("PFX ")
							.precedes(tag(flag.to_string().as_str()))
							.precedes(|i| {
								// 0 means nothing
								let (i, strip) = is_not(" ")
									.preceded_by(space1)
									// TODO
									.map(|s| if s == "0" { "" } else { s })
									// .map(|s| if s == "0" { None } else { Some(s) })
									.parse(i)?;

								let (i, (add, flags)) = tuple((
									// TODO: handle escaped affix `\/` ?
									is_not("/ \n")
										// .map(|s| if s == "0" { None } else { Some(s) })
										.map(|s| if s == "0" { "" } else { s }),
									opt(Self::parse_flags(&options).preceded_by(tag("/"))),
								))
								.preceded_by(space1)
								.parse(i)?;

								// . means nothing
								let (i, condition) = is_not(" \n")
									.opt()
									.map(|s| if s == Some(".") { None } else { s })
									.preceded_by(space1)
									// TODO: is this really opt? ~spec is not clear cause dot is placeholder
									.parse(i)?;

								let (i, data_fields) = is_not("\n")
									.preceded_by(space1)
									.opt()
									// TODO: actually parse data fields
									.value(vec![])
									.parse(i)?;

								let pfx = Affix::<Prefix> {
									flag,
									cross_product,

									strip: strip.to_owned(),
									add: add.to_owned(),
									condition: condition
										.map(|s| Regex::new(&format!("^{s}")).unwrap()),

									flags: Flags::new(flags.unwrap_or_default()),
									data_fields,

									kind: PhantomData,
								};

								log::debug!("(PFX) added prefix {}", pfx);
								Ok((i, pfx))
							})
							.terminated(space0)
							.terminated(newline),
					)(i)?;

					(i, prefixes.append(&mut pfxs))
				}
				"SFX" => {
					let (i, (flag, cross_product, num)) = tuple((
						Self::parse_flag(&options.flag_ty),
						map(take(1_usize).preceded_by(space1), |s| {
							Self::parse_cross_product(s).unwrap()
						}),
						u64_p.preceded_by(space1).terminated(newline),
					))(i)?;
					let (i, mut sfxs) = many_m_n(
						usize::try_from(num).unwrap(),
						usize::try_from(num).unwrap(),
						tag("SFX ")
							.precedes(tag(flag.to_string().as_str()))
							.precedes(|i| {
								// 0 means nothing
								let (i, strip) = is_not(" ")
									.preceded_by(space1)
									// TODO
									.map(|s| if s == "0" { "" } else { s })
									// .map(|s| if s == "0" { None } else { Some(s) })
									.parse(i)?;

								let (i, (add, flags)) = tuple((
									// TODO: handle escaped affix `\/` ?
									is_not("/ \n")
										// .map(|s| if s == "0" { None } else { Some(s) })
										.map(|s| if s == "0" { "" } else { s }),
									opt(Self::parse_flags(&options).preceded_by(tag("/"))),
								))
								.preceded_by(space1)
								.parse(i)?;

								// . means nothing
								let (i, condition) = is_not(" \n")
									.opt()
									.map(|s| if s == Some(".") { None } else { s })
									.preceded_by(space1)
									// TODO: is this really opt? ~spec is not clear cause dot is placeholder
									.parse(i)?;

								let (i, data_fields) = is_not("\n")
									.preceded_by(space1)
									.opt()
									// TODO: actually parse data fields
									.value(vec![])
									.parse(i)?;

								let sfx = Affix::<Suffix> {
									flag,
									cross_product,

									strip: strip.to_owned(),
									add: add.to_owned(),
									condition: condition
										.map(|s| Regex::new(&format!("{s}$")).unwrap()),

									flags: Flags::new(flags.unwrap_or_default()),
									data_fields,

									kind: PhantomData,
								};

								log::debug!("(SFX) added suffix {}", sfx);
								Ok((i, sfx))
							})
							.terminated(space0)
							.terminated(newline),
					)(i)?;

					(i, suffixes.append(&mut sfxs))
				}

				// ——— other
				"CIRCUMFIX" => {
					let (i, flag) = Self::parse_flag(&options.flag_ty)(i)?;
					(i, set_flag(&mut additional_flags.circum_fix, flag))
				}
				"FORBIDDENWORD" => {
					let (i, flag) = Self::parse_flag(&options.flag_ty)(i)?;
					(i, set_flag(&mut additional_flags.forbidden_word, flag))
				}
				"FULLSTRIP" => (i, options.full_strip = true),
				"KEEPCASE" => {
					let (i, flag) = Self::parse_flag(&options.flag_ty)(i)?;
					(i, set_flag(&mut additional_flags.keep_case, flag))
				}

				"ICONV" => {
					let (i, num) = u64_p.terminated(newline).parse(i)?;
					let (i, conversions) = many_m_n(
						usize::try_from(num).unwrap(),
						usize::try_from(num).unwrap(),
						move |i: &'a str| -> IResult<&'a str, (&'a str, &'a str)> {
							let (i, pattern) = preceded(tag("ICONV "), is_not(" "))(i)?;
							let (i, rep_pattern) =
								preceded(tag(" "), terminated(is_not("\n"), newline))(i)?;

							Ok((i, (pattern, rep_pattern)))
						},
					)(i)?;

					for (pattern, rep) in conversions {
						options.input_conversion.add(pattern, rep);
					}
					(i, ())
				}
				"OCONV" => {
					let (i, num) = terminated(u64_p, newline)(i)?;
					let (i, conversions) = many_m_n(
						usize::try_from(num).unwrap(),
						usize::try_from(num).unwrap(),
						|i: &'a str| -> IResult<&'a str, (&'a str, &'a str)> {
							let (i, pattern) = preceded(tag("OCONV "), is_not(" "))(i)?;
							let (i, rep_pattern) =
								preceded(tag(" "), terminated(is_not("\n"), newline))(i)?;

							Ok((i, (pattern, rep_pattern)))
						},
					)(i)?;

					for (pattern, rep) in conversions {
						options.input_conversion.add(pattern, rep);
					}

					(i, ())
				}

				"LEMMA_PRESENT" => todo!("LEMMA_PRESENT"),
				"NEEDAFFIX" => {
					let (i, flag) = Self::parse_flag(&options.flag_ty)(i)?;
					(i, set_flag(&mut additional_flags.need_affix, flag))
				}

				"PSEUDOROOT" => {
					// TODO: ugly
					eprintln!("warn: pseudo root");

					let (i, flag) = Self::parse_flag(&options.flag_ty)(i)?;
					(i, set_flag(&mut additional_flags.need_affix, flag))
				}
				"SUBSTANDARD" => {
					let (i, flag) = Self::parse_flag(&options.flag_ty)(i)?;
					(i, set_flag(&mut additional_flags.sub_standard, flag))
				}
				"WORDCHARS" => {
					let (i, chars) = chars_till_end(i)?;
					(i, options.word_chars = chars)
				}
				"CHECKSHARPS" => (i, options.check_sharps = true),

				"#" => (i, ()),

				unknown_dir => todo!("warn unknown directive: {unknown_dir}"),
			};

			Ok(res)
		}
	}

	/// Parse a cross product character
	fn parse_cross_product(s: &str) -> Option<bool> {
		match s {
			"Y" => Some(true),
			"N" => Some(false),
			_ => None,
		}
	}

	/// Parse a flag
	fn parse_flag(fty: &FlagType) -> impl Fn(&str) -> IResult<&str, Flag> + '_ {
		move |i: &str| match &fty {
			FlagType::Short => satisfy(|c| c.is_ascii() && !c.is_ascii_whitespace())
				.map(Flag::Short)
				.parse(i),
			FlagType::Long => satisfy(|c| c.is_ascii() && !c.is_ascii_whitespace())
				.array()
				.map(Flag::Long)
				.parse(i),
			FlagType::Utf8 => satisfy(|c| !c.is_whitespace()).map(Flag::Utf8).parse(i),
			FlagType::Numeric => u16_p.map(Flag::Numeric).parse(i),
		}
	}

	/// Parse a list of flags
	fn parse_flags<'a>(options: &'a Options) -> impl Fn(&str) -> IResult<&str, Vec<Flag>> + 'a {
		move |i: &str| {
			{
				// TODO: this does not check `strip/2,3,4`
				let (i, digit) = opt(u64_p)(i)?;

				if !options.flag_aliases.is_empty() {
					if let Some(digit) = digit {
						return Ok((
							i,
							// TODO: errors must be really bad
							// wouldn't like to debug that later
							options
								.flag_aliases
								// We translate to a 0-index table
								.get(usize::try_from(digit - 1).unwrap())
								.unwrap()
								.clone(),
						));
					}
				}
			}

			match options.flag_ty {
				FlagType::Short | FlagType::Long | FlagType::Utf8 => {
					many1(Self::parse_flag(&options.flag_ty))(i)
				}
				FlagType::Numeric => {
					separated_list1(tag(","), Self::parse_flag(&options.flag_ty))(i)
				}
			}
		}
	}

	/// Parse stem associated data fields
	fn parse_data_fields(i: &str) -> IResult<&str, Vec<DataField>> {
		separated_list1(space1, Self::parse_data_field.map(|df| df))(i)
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

			invalid_tag => todo!("error out with invalid data field type: {invalid_tag}"),
		}
	}
}

// TODO: remove
// Reexport to parse flags in the dictionary
#[doc(hidden)]
pub(crate) fn parse_flags(options: &Options) -> impl Fn(&str) -> IResult<&str, Vec<Flag>> + '_ {
	AffParser::parse_flags(options)
}
// Reexport to parse data fields in the dictionary
#[doc(hidden)]
pub(crate) fn parse_data_field(i: &str) -> IResult<&str, DataField> {
	AffParser::parse_data_field(i)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Flags(Vec<Flag>);

impl Flags {
	pub(crate) fn new(flags: Vec<Flag>) -> Self {
		Self(flags)
	}

	pub(crate) fn has_some(&self) -> bool {
		!self.0.is_empty()
	}

	pub(crate) fn has_flag(&self, flag: &Flag) -> bool {
		self.0.contains(flag)
	}
}

impl fmt::Display for Flags {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		for flag in &self.0 {
			write!(f, "{flag}")?;
		}
		Ok(())
	}
}

/// A flag in dictionary files, see
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Flag {
	/// `short`: flag is a single ascii character
	///
	/// e.g. `A`
	Short(char),
	/// `long`: flag is defined as two ascii character
	///
	/// e.g. `AB`
	Long([char; 2]),
	/// `UTF-8`: flag can be any character from the utf8 set
	///
	/// e.g. `π`
	Utf8(char),
	/// `num`: flag is a number in 1-65000. Multiple flags are
	/// separated by a comma.
	///
	/// e.g. `12345`
	Numeric(u16),
}

impl fmt::Display for Flag {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Short(char) | Self::Utf8(char) => write!(f, "{char}"),
			Self::Long([c1, c2]) => write!(f, "{c1}{c2}"),
			Self::Numeric(num) => write!(f, "{num}"),
		}
	}
}

#[derive(Debug, Default)]
pub(crate) struct IgnoreList(Vec<char>);

impl IgnoreList {
	pub(crate) fn erase<'a>(&self, word: &mut String) {
		word.retain(|c| !self.0.contains(&c));
	}
}

/// How flags should be parsed.
///
/// See [`Flag`] to see all forms with examples
#[derive(Debug, Default, Clone)]
pub(crate) enum FlagType {
	/// `short`
	#[default]
	Short,
	/// `long`
	Long,
	/// `UTF-8`
	Utf8,
	/// `num` (or `numeric`?)
	Numeric,
}

impl FromStr for FlagType {
	type Err = ();

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		match s {
			"short" => Ok(Self::Short),
			"long" => Ok(Self::Long),
			// TODO: check which one is it
			"num" | "numeric" => Ok(Self::Numeric),
			"UTF-8" => Ok(Self::Utf8),
			_ => Err(()),
		}
	}
}

/// Generic because used with `Flag`, `Vec<Flag>`
fn set_flag<T: Debug>(place: &mut Option<T>, flag: T) {
	if let Some(old_flag) = place.replace(flag) {
		// TODO: warn
		todo!("warn: flag was replaced {old_flag:?}")
	};
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn parse_iconv_directive() -> Result<(), Box<dyn std::error::Error>> {
		let directive = "ICONV 1\nICONV ' `\n";
		let mut parser = AffParser::default();

		let remains = AffParser::parse_directive(&mut parser)(directive)?.0;
		assert!(remains.is_empty());

		assert!(parser.options.input_conversion.replacements.len() == 1);

		Ok(())
	}

	#[test]
	fn find_suffixes_in_index() -> Result<(), Box<dyn std::error::Error>> {
		let directive = "
SFX D Y 4
SFX D   y     ied        [^aeiou]y
SFX D   0     ed         [^ey]
SFX D   0     ed         [aeiou]y
SFX D   0     d          e
";
		let parser = AffFile::new(directive)?;

		let searched_word = "respelled".chars().rev().collect::<String>();

		let results = parser.suffix_index.get_all(&searched_word);

		assert_eq!(results.len(), 3);

		Ok(())
	}

	#[test]
	fn parse_data_field_forms() -> Result<(), nom::Err<nom::error::Error<&'static str>>> {
		macro_rules! test {
			($source:literal => $res:expr) => {{
				use DataField::*;
				let (i, df) = AffParser::parse_data_field($source)?;
				assert_eq!(df, $res);
				assert_eq!(i, "");
			}};
		}

		let test_word = String::from("hello");

		test!("ph:hello" => Alternative(test_word.clone()));
		// TODO
		// test_df!("ph:hello->bye" => Alternative(test_word));

		test!("st:hello" => Stem(test_word.clone()));
		test!("al:hello" => Allomorph(test_word.clone()));
		test!("po:hello" => PartOfSpeech(test_word.clone()));

		test!("ds:hello" => DerivationalSuffix(test_word.clone()));
		test!("is:hello" => InflectionalSuffix(test_word.clone()));
		test!("ts:hello" => TerminalSuffix(test_word.clone()));

		test!("dp:hello" => DerivationalPrefix(test_word.clone()));
		test!("ip:hello" => InflectionalPrefix(test_word.clone()));
		test!("tp:hello" => TerminalPrefix(test_word.clone()));

		test!("sp:hello" => SurfacePrefix(test_word.clone()));
		test!("pa:hello" => PartsOfCompound(test_word));

		Ok(())
	}

	#[test]
	fn parse_data_fields() -> Result<(), nom::Err<nom::error::Error<&'static str>>> {
		use DataField::*;
		macro_rules! test {
			($source:literal => $res:expr) => {{
				let (i, s) = AffParser::parse_data_fields($source)?;
				assert_eq!(s.as_slice(), $res.as_slice());
				assert_eq!(i, "");
			}};
		}

		let test_word = String::from("hello");

		test!("ph:hello al:hello" => [Alternative(test_word.clone()), Allomorph(test_word.clone())]);
		test!("ph:hello al:hello po:hello" => [Alternative(test_word.clone()), Allomorph(test_word.clone()), PartOfSpeech(test_word)]);
		// TODO
		// test_df!("ph:hello->bye" => Alternative(hello.clone()));

		Ok(())
	}
}
