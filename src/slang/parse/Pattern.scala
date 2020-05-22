package slang.parse

import slang.lex.Token
import slang.runtime.Value

sealed trait Pattern

object Pattern {

  case class Id(name: Token) extends Pattern

  case class Ignore(token: Token) extends Pattern

  case class Lazy(inner: Pattern) extends Pattern

  case class Literal(token: Token, value: Value) extends Pattern

  case class SlangList(patterns: List[Pattern]) extends Pattern

  case class Spread(name: Token) extends Pattern

}