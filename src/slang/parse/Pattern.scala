package slang.parse

import slang.lex.{Token, TokenType}
import slang.runtime._

sealed trait Pattern {
  def toSlangString: String

  def isHashable: Boolean = false
  def asHashable: Value = ???
}

object Pattern {

  case class Id(name: Token) extends Pattern {
    override def toSlangString(): String = name.lexeme
  }

  case class Ignore(token: Token) extends Pattern {
    override def toSlangString(): String = "_"
  }

  case class Strict(inner: Pattern, full: Boolean) extends Pattern {
    override def toSlangString(): String = "{" + inner.toSlangString + "}"
  }

  case class Literal(value: Value) extends Pattern {
    override def toSlangString: String = value.toSlangString

    override def isHashable: Boolean = true
    override def asHashable: Value = value
  }

  case class List(patterns: scala.List[Pattern]) extends Pattern {
    override def toSlangString: String = "[" + (patterns map { _.toSlangString } mkString ", ") + "]"

    override def isHashable: Boolean = patterns forall { _.isHashable }
    override def asHashable: Value = Value.List(patterns map { _.asHashable })
  }

  case class Cons(head: Pattern, tail: Pattern) extends Pattern {
    override def toSlangString: String = "(" + head.toSlangString + " . " + tail.toSlangString + ")"

    override def isHashable: Boolean = false // TODO(chris): could probably inspect the tail for hashable patterns
  }

  case class Spread(name: Token) extends Pattern {
    override def toSlangString: String = name.lexeme
  }
}