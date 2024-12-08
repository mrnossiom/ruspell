//! Ruspell CLI
//!
//! Historic~ test words: `worstershirebied`, `ReWhalas`

use clap::Parser;
use ruspell::Dictionary;
use std::{
	io::{stdin, stdout, Write},
	path::Path,
};

#[derive(clap::Parser)]
struct Args {
	word: Option<String>,

	#[arg(long, short)]
	interactive: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
	pretty_env_logger::init();

	let args = Args::parse();

	let dict = Dictionary::from_pair(Path::new(env!("HUNSPELL_DICT")))?;

	if let Some(word) = args.word {
		lookup_and_print(&dict, &word);
		return Ok(());
	}

	if args.interactive {
		loop {
			print!("lookup word(s) ❯ ");
			stdout().flush().unwrap();

			let mut input = String::new();
			if let 0 = stdin().read_line(&mut input)? {
				std::process::exit(0);
			};
			let input = input.trim();

			input.split_whitespace().for_each(|word| {
				lookup_and_print(&dict, word);
			});
		}
	} else {
		return Err("no action provided".into());
	}

	Ok(())
}

fn lookup_and_print(dict: &Dictionary, word: &str) {
	match dict.lookup(word) {
		Ok(true) => log::info!("Word `{word}` was found in the dictionary"),
		Ok(false) => log::warn!("Word `{word}` wasn't found in the dictionary"),
		Err(err) => log::error!("Could not lookup `{word}`: {err}"),
	}
}
