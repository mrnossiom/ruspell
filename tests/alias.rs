mod utils;

#[test]
fn alias_one() -> Result<(), Box<dyn std::error::Error>> {
	utils::test_dictionary_pair(
		"\
# aliases for flag vectors (AF)
# AB -> 1
# A -> 2
AF 2
AF AB
AF A

SFX A Y 1
SFX A 0 x .

SFX B Y 1
SFX B 0 y/2 .
",
		"\
1
foo/1
",
		&["foo", "foox", "fooy", "fooyx"],
		&[],
		None,
	)
}

#[test]
#[ignore]
fn alias_two() -> Result<(), Box<dyn std::error::Error>> {
	utils::test_dictionary_pair(
		"\
# aliases for flag vectors (AF) and morphological descriptions (AM)
# AB -> 1
# A -> 2
AF 2
AF AB
AF A

AM 3
AM is:affix_x
AM ds:affix_y
AM po:noun xx:other_data

SFX A Y 1
SFX A 0 x . 1

SFX B Y 1
SFX B 0 y/2 . 2
",
		"\
1
foo/1	3
",
		&["foo", "foox", "fooy", "fooyx"],
		&[],
		None,
	)
}
