//! Basic Trie data structure implementation which is used in multiple places

use std::collections::HashMap;

/// Trie data strucutre
#[derive(Debug)]
pub(crate) struct Trie<V> {
	/// Tree root
	root: TrieNode<V>,
	/// Number of items present in the Trie
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

/// A [`Trie`] node
#[derive(Debug)]
struct TrieNode<V> {
	/// Character mapped to children nodes
	leaves: HashMap<char, Self>,
	/// - `None` if node is only a path to others
	/// - `Some` if node is significant and has associated data
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
	/// Get total numbers of items in [`Trie`]
	const fn len(&self) -> usize {
		self.len
	}

	/// Insert an item in the tree based on its key
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

	/// Return every value associated on the path of the given key
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

	/// Return every key and its associated on the path of the given key
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
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn get() {
		let mut trie = Trie::default();
		trie.insert("pr", ());
		trie.insert("pre", ());
		trie.insert("pred", ());
		trie.insert("preda", ());

		let results = trie.get_all("pred");
		assert_eq!(results.len(), 3);
	}
}
