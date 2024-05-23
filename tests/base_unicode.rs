mod utils;

const BASE_AFF: &str = include_str!("./base_unicode.aff");

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
can’t
doesn’t
etc
won’t
lip
text
horrifying
speech
suggest
uncreate/V
Hunspell
İzmir
";

const BASE_GOOD: [&str; 33] = [
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
	"can’t",
	"doesn’t",
	"won’t",
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
	"İzmir",
	"İZMİR",
	"İzmir.",
	"İZMİR.",
	"Imply",
	"IMPLY",
];

const BASE_WRONG: [&str; 13] = [
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
	"İmply",
	"İMPLY",
];

#[test]
#[ignore = "implement locale specific lowercasing"]
fn base_unicode() -> Result<(), Box<dyn std::error::Error>> {
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
		vec!["Imply"],
		vec!["IMPLY"],
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
