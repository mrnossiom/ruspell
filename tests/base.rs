mod utils;

const BASE_AFF: &str = include_str!("./base.aff");

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

#[test]
fn base() -> Result<(), Box<dyn std::error::Error>> {
	let suggestions = vec![
		vec!["looked", "look"],
		vec!["text"],
		vec!["hello"],
		vec!["said"],
		vec!["rotten day", "rotten-day", "rotten"],
		vec!["tomorrow"],
		vec!["seven"],
		vec!["NASA"],
		vec!["horrifying"],
		vec!["speech"],
		vec!["suggest"],
	];

	utils::test_dictionary_pair(
		BASE_AFF,
		BASE_DIC,
		&BASE_GOOD,
		&BASE_WRONG,
		// Some(&suggestions),
		None,
	)
}
