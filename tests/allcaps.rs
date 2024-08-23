mod utils;

/// Check uppercase forms of allcaps word + affix and words with mixed casing
#[test]
fn alias_one() -> Result<(), Box<dyn std::error::Error>> {
	utils::test_dictionary_pair(
		"\
WORDCHARS '.

SFX S N 1
SFX S   0     's      .
",
		"\
2
OpenOffice.org
UNICEF/S
",
		&["OpenOffice.org", "OPENOFFICE.ORG", "UNICEF's", "UNICEF'S"],
		&[],
		None,
	)
}

/// Check uppercase forms of allcaps word + affix and words with mixed casing
#[test]
fn alias_two() -> Result<(), Box<dyn std::error::Error>> {
	utils::test_dictionary_pair(
		"\
# forbidden all caps words are case sensitive
# iPod -> ipodos ('iPodic' in Hungarian)
FORBIDDENWORD *
SFX s N 1
SFX s 0 os .
",
		"\
3
iPod/s
iPodos/*
ipodos
",
		&["iPod", "IPOD", "ipodos", "IPODOS"],
		&[],
		None,
	)
}
