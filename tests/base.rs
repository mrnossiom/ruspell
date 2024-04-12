use ruspell::Dictionary;

const BASE_AFFIX: &str = include_str!("./en_US.aff");
const BASE_DIC: &str = "\
28
created/U
create/XKVNGADS
imply/GNSDX
natural/PUY
like/USPBY
convey/BDGS
look/GZRDS
text
hello
said
sawyer
NASA
rotten
day
tomorrow
seven
FAQ/SM
can't
doesn't
etc
won't
lip
text
horrifying
speech
suggest
uncreate/V
Hunspell
";

#[test]
fn base() -> Result<(), Box<dyn std::error::Error>> {
	let dict = Dictionary::from_slice(BASE_AFFIX, BASE_DIC)?;

	Ok(())
}
