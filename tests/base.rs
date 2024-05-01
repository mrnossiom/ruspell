use ruspell::Dictionary;

const BASE_AFF: &str = include_str!("./en_US.aff");

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

const BASE_GOOD: [&str; 27] = [
	"created",
	"uncreate",
	"uncreated",
	"imply",
	"implied",
	"unnatural",
	"conveyed",
	"sawyer",
	"NASA",
	"FAQs",
	"can't",
	"doesn't",
	"won't",
	"Created",
	"Hello",
	"HELLO",
	"NASA",
	"etc.",
	"etc",
	"HELLO",
	"lip.",
	"text.",
	"NASA.",
	"Text.",
	"TEXT.",
	"Hunspell.",
	"HUNSPELL.",
];

const BASE_WRONG: [&str; 11] = [
	"loooked",
	"texxt",
	"hlelo",
	"seid",
	"rottenday",
	"tomorow",
	"seeeven",
	"Nasa",
	"horrorfying",
	"peech",
	"sugesst",
];

const BASE_WRONG_SUGG: [&str; 11] = [
	"looked, look",
	"text",
	"hello",
	"said",
	"rotten day, rotten-day, rotten",
	"tomorrow",
	"seven",
	"NASA",
	"horrifying",
	"speech",
	"suggest",
];

#[test]
#[ignore]
fn base() -> Result<(), Box<dyn std::error::Error>> {
	let dict = Dictionary::from_slice(BASE_AFF, BASE_DIC)?;

	let mut pass = true;

	BASE_GOOD
		.into_iter()
		.filter(|w| !dict.lookup(w).unwrap_or_default())
		.for_each(|ww| {
			pass = false;
			eprintln!("{ww} is supposed to be fine but is wrong");
		});

	BASE_GOOD
		.into_iter()
		.filter(|w| dict.lookup(w).unwrap_or_default())
		.for_each(|ww| {
			pass = false;
			eprintln!("{ww} is supposed to be wrong but is fine");
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
