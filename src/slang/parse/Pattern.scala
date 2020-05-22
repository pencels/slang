package slang.parse

import slang.lex.Token
import slang.runtime.{Environment, Value}

sealed trait Pattern {
  def toSlangString: String
}

object Pattern {

  case class Id(name: Token) extends Pattern {
    override def toSlangString(): String = name.lexeme
  }

  case class Ignore(token: Token) extends Pattern {
    override def toSlangString(): String = "_"
  }

  case class Strict(inner: Pattern) extends Pattern {
    override def toSlangString(): String = "{" + inner.toSlangString + "}"
  }

  case class Literal(token: Token, value: Value) extends Pattern {
    override def toSlangString: String = value.toSlangString
  }

  case class SlangList(patterns: List[Pattern]) extends Pattern {
    override def toSlangString: String = "[" + (patterns map { _.toSlangString } mkString ", ") + "]"
  }

  case class Spread(name: Token) extends Pattern {
    override def toSlangString: String = name.lexeme + ".."
  }
}