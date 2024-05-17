use ruspell::Dictionary;

#[derive(Debug, thiserror::Error)]
#[error("{0} word failed to be correctly spellchecked")]
struct SpellCheckErrors(usize);

pub(crate) fn test_dictionary_pair(
	aff: &str,
	dic: &str,
	good: &[&str],
	wrong: &[&str],
	suggestions: Option<&[Vec<&str>]>,
) -> Result<(), Box<dyn std::error::Error>> {
	pretty_env_logger::init();

	let dict = Dictionary::from_slice(aff, dic)?;

	let mut errrors = 0;

	errrors += good
		.iter()
		.filter(|w| {
			if dict.lookup(w).unwrap_or_default() {
				log::info!("{w} is indeed fine");
				false
			} else {
				log::error!("{w} is supposed to be fine but is wrong");
				true
			}
		})
		.count();

	errrors += wrong
		.iter()
		.filter(|w| {
			if dict.lookup(w).unwrap_or_default() {
				log::error!("{w} is supposed to be wrong but is fine");
				true
			} else {
				log::info!("{w} is indeed wrong");
				false
			}
		})
		.count();

	if let Some(suggs) = suggestions {
		assert_eq!(suggs.len(), wrong.len());

		todo!()
	}

	if errrors == 0 {
		Ok(())
	} else {
		Err(Box::new(SpellCheckErrors(errrors)))
	}
}
