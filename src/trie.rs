use std::collections::HashMap;

#[derive(Debug)]
pub(crate) struct Trie<V> {
	root: TrieNode<V>,
	len: usize,
}

impl<V> Default for Trie<V> {
	fn default() -> Self {
		Self {
			root: TrieNode::default(),
			len: 0,
		}
	}
}

#[derive(Debug)]
struct TrieNode<V> {
	leaves: HashMap<char, Self>,
	data: Option<Vec<V>>,
}

impl<V> Default for TrieNode<V> {
	fn default() -> Self {
		Self {
			leaves: HashMap::default(),
			data: None,
		}
	}
}

impl<V> Trie<V> {
	const fn len(&self) -> usize {
		self.len
	}

	pub(crate) fn insert(&mut self, key: &str, value: V) {
		let mut current = &mut self.root;
		for char in key.chars() {
			current = current.leaves.entry(char).or_default();
		}

		match &mut current.data {
			None => current.data = Some(vec![value]),
			Some(ref mut vec) => vec.push(value),
		}
	}

	pub(crate) fn get_all_values<'a>(&'a self, key: &'a str) -> Vec<(&'a str, &'a V)> {
		let mut current = &self.root;
		let mut results = vec![];

		for (index, char) in key.char_indices() {
			current = match current.leaves.get(&char) {
				Some(v) => v,
				None => break,
			};

			match &current.data {
				Some(val) => results.extend(val.iter().map(|v| (&key[..index], v))),
				None => continue,
			};
		}

		results
	}

	pub(crate) fn get_all<'a>(&'a self, key: &'a str) -> Vec<&'a V> {
		let mut current = &self.root;
		let mut results = vec![];

		for char in key.chars() {
			current = match current.leaves.get(&char) {
				Some(v) => v,
				None => break,
			};

			match &current.data {
				Some(val) => results.extend(val),
				None => continue,
			};
		}

		results
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn can_create_and_search() {
		let mut trie = Trie::default();
		trie.insert("pr", ());
		trie.insert("pre", ());
		trie.insert("pred", ());
		trie.insert("preda", ());

		let results = trie.get_all("pred");
		assert_eq!(results.len(), 3);
	}
}
