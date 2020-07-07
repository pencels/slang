package slang.lex

import scala.collection._

case class OperatorTrie(var end: Boolean, children: mutable.HashMap[Char, OperatorTrie]) {
    def find(op: String): Option[OperatorTrie] = {
        op.headOption match {
            case Some(head) => children.get(head).flatMap(_.find(op))
            case None => Some(this)
        }
    }

    def add(op: String): Unit = {
        op.headOption match {
            case Some(head) => children.getOrElseUpdate(head, OperatorTrie.empty).add(op.tail)
            case None => end = true
        }
    }

    def get(char: Char): Option[OperatorTrie] = children.get(char)
}

object OperatorTrie {
    def empty() = OperatorTrie(false, new mutable.HashMap[Char, OperatorTrie])
}