package org.matmexrhino.datastructures

import org.scalacheck._
import Prop._

object CharTrieSpecification extends Properties("Character Trie") {
  val trie = new CharTrie

  property("should roundtrip") = forAll { (s: String) =>
    trie.str(trie.insert(s)) == s
  }   
}
