//! Ruspell CLI

use ruspell::Dictionary;
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let dict = Dictionary::from_pair(Path::new(env!("HUNSPELL_DICT")))?;

	dbg!(dict.lookup("worstershirebied")?);

	Ok(())
}
