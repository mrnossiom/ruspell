use crate::{aff::AffFile, dic::DicFile};
use std::{io, path::Path};

pub struct Dictionary {
	pub(crate) aff: AffFile,
	pub(crate) dic: DicFile,
}

#[derive(Debug, thiserror::Error)]
pub enum InitializeError {
	#[error("Could not parse file: {0}")]
	Parser(String),

	#[error(transparent)]
	Io(#[from] io::Error),
}

/// Constructors
impl Dictionary {
	/// # Errors
	///
	/// Will error if either the provided affix or dictionary file are not able
	/// to be parsed.
	pub fn from_slice(aff: &str, dic: &str) -> Result<Self, InitializeError> {
		let aff = AffFile::new(aff)?;
		let dic = DicFile::new(dic, &aff.options)?;
		Ok(Self { aff, dic })
	}

	/// Given a path `/path/to/hunspell/en_US`, this function will append `.aff`
	/// and `.dic` and then read those files.
	///
	/// # Errors
	///
	/// Will error if either the provided affix or dictionary file are not able
	/// to be parsed. It will fail too if the function failed to read the given
	/// path.
	pub fn from_pair(base: &Path) -> Result<Self, InitializeError> {
		let aff = AffFile::file(&base.with_extension("aff"))?;
		let dic = DicFile::file(&base.with_extension("dic"), &aff.options)?;
		Ok(Self { aff, dic })
	}

	/// Hunspell dictionaries are sometimes distributed as `zip` archives containing
	/// both the dictionary and affix file.
	///
	/// This is the case for LibreOffice (`.odt`) and Firefox (`.xpi`) dictionaries.
	///
	/// # Errors
	///
	/// Will error if either the found affix or dictionary file are not able
	/// to be parsed. It will fail too if the provided zip does not exists or
	/// contain both required files.
	pub fn from_zip(_path: &Path) -> Result<Self, InitializeError> {
		todo!("ruspell cannot import zip files yet")
	}
}
