package org.matmexrhino.datastructures

import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

class CharTrie {
  object IntList {
    val InitSize = 2
    val GrowBy = 7
  }

  // Stores chars only! - Can I save some memory?
  class IntList {
    import IntList._
    private var elems = new Array[Int](InitSize)
    elems(elems.length - 1) = -InitSize

    def insert(idx: Int, elem: Int) {
      val last = elems(elems.length - 1)
      if (last < 0) {
	assert(idx <= size, "Can't insert at non-continuous position")
	elems(elems.length - 1) += 1
	var pos = elems.length + last - 1
	while (pos >= idx) {
          elems(pos + 1) = elems(pos)
          pos -= 1
	}
	elems(idx) = elem
      } else {
	assert(idx <= elems.length, "Can't insert at non-continuous position")
	val newElems = new Array[Int](elems.length + GrowBy)
	newElems(newElems.length - 1) = 1 - GrowBy
	if (idx > 0) Array.copy(elems, 0, newElems, 0, idx)
	newElems(idx) = elem
	if (idx < elems.length) Array.copy(elems, idx, newElems, idx + 1, elems.length - idx)
	elems = newElems
      }
    }

    override def toString = "IntList(" + elems.mkString(" ") + ")"
    def size = {
      val last = elems(elems.length - 1)
      if (last < 0) elems.length + last else elems.length
    }
    def at(idx: Int) = elems(idx)
  }

  abstract class Node {
    lazy val children = new IntList
  }
  case class CharNode(c: Char, parent: Int) extends Node
  case object Root extends Node

  val buff = new ArrayBuffer[Node]
  buff += Root

  def insert(cs: Seq[Char]): Int = {
    var idx = 0
    var curr = 0
    for (c <- cs) {
      curr = insert0(curr, c)
      idx += 1
    }
    curr
  }

  private def insert0(n: Int, c: Char): Int = {
    val children = buff(n).children
    def doInsert(idx: Int) = {
      val pos = size
      children.insert(idx, pos)
      buff += new CharNode(c, n)
      pos
    }
    @tailrec def insert1(left: Int, right: Int): Int = {
      val middle = (left + right) >> 1
      val childPos = children.at(middle)
      val cc = (buff(childPos): @unchecked) match {
	case CharNode(cc, _) => cc
      }
      if (cc == c) childPos
      else if (middle == left) doInsert(right)
      else if (cc < c) insert1(middle, right) else insert1(left, middle)
    }
    if (children.size == 0) doInsert(0) else insert1(0, children.size)
  }

  def str(hash: Int) = {
    assert(hash >= 0 && hash < buff.length, "Invalid index to CharTrie: " + hash)
    str0(buff(hash), new StringBuilder)
  }
  @tailrec private def str0(n: Node, builder: StringBuilder): String = n match {
    case Root => builder.toString.reverse
    case CharNode(c, p) => str0(buff(p), builder append c)
  }
  def size = buff.size
  def clear() { buff.clear() }
  override def toString = buff.mkString(",")
}
