package slang.lex

import scala.collection._

class OperatorTrie {
  val root = OperatorTrieNode.empty
  var active = true

  def add(operator: String) = root.add(operator)
  def get(char: Char) = root.get(char)
}

object OperatorTrieNode {
  def empty = OperatorTrieNode(mutable.Map(), false)
}

case class OperatorTrieNode(
    children: mutable.Map[Char, OperatorTrieNode],
    var end: Boolean
) {
  def add(operator: String): Unit = {
    operator.headOption match {
      case Some(head) =>
        children
          .getOrElseUpdate(head, OperatorTrieNode.empty)
          .add(operator.tail)
      case None => end = true
    }
  }

  def get(char: Char) = children.get(char)
}
