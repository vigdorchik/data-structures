package org.matmexrhino.datastructures

import org.scalacheck._
import Prop._

object CharTrieSpecification extends Properties("Character Trie") {
  val trie = new CharTrie

  property("roundtrip") = forAll { (s: String) =>
    val hash = trie.insert(s)
    ("Get back inserted" |: trie.str(hash) == s) &&
    ("Idempotent"        |: trie.insert(s) == hash)
  }
}
