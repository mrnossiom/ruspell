use ruspell::Dictionary;

const FULLSTRIP_AFF: &str = "\
# FULLSTRIP option: Hunspell can strip full words by affix rules
# see OpenOffice.org Issue #80145
# test data from Davide Prina

FULLSTRIP

SET ISO8859-15
TRY aioertnsclmdpgubzfvhàq'ACMSkBGPLxEyRTVòIODNwFéùèìjUZKHWJYQX

SFX A Y 3
SFX A andare vado andare
SFX A andare va andare
SFX A are iamo andare

# SFX A Y 3 # verbo andare (verb to go)
# SFX A andare vado andare # io vado (I go)
# SFX A andare va andare # tu vai (you go)
# SFX A are iamo andare # noi andiamo (we go)
";

const FULLSTRIP_DIC: &str = "\
2
andare/A
riandare/A
";

const FULLSTRIP_GOOD: [&str; 8] = [
	"andare",
	"vado",
	"va",
	"andiamo",
	"riandare",
	"rivado",
	"riva",
	"riandiamo",
];

#[test]
fn fullstrip() -> Result<(), Box<dyn std::error::Error>> {
	let dict = Dictionary::from_slice(FULLSTRIP_AFF, FULLSTRIP_DIC)?;

	let mut pass = true;

	FULLSTRIP_GOOD
		.into_iter()
		.filter(|w| !dict.lookup(w).unwrap_or_default())
		.for_each(|ww| {
			pass = false;
			eprintln!("{ww} is supposed to be fine but is wrong");
		});

	if pass {
		Ok(())
	} else {
		Err(Box::new(std::io::Error::new(
			std::io::ErrorKind::NotFound,
			"",
		)))
	}
}
