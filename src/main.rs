use ruspell::Dictionary;
use std::path::Path;

fn main() {
	let _dict = Dictionary::from_pair(Path::new(env!("HUNSPELL_DICT"))).unwrap();
}
