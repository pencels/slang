package slang.lex

import java.lang.{String => JString}

import slang.sourcemap.Span

case class Token(span: Span, ty: TokenType)

sealed trait TokenType

/**
  * A `TokenType` which does not contain any information other than what kind of token it is.
  */
sealed trait SimpleToken extends TokenType

object TokenType {
  case class Id(name: JString) extends TokenType
  case class TypeId(name: JString) extends TokenType

  case object Nothing extends SimpleToken
  case class Atom(name: JString) extends TokenType
  case class Number(num: Double) extends TokenType
  case class String(str: JString) extends TokenType
  case class Op(op: JString) extends TokenType

  case object Let extends SimpleToken
  case object When extends SimpleToken
  case object Operator extends SimpleToken
  case object Fn extends SimpleToken

  case object LParen extends SimpleToken
  case object RParen extends SimpleToken
  case object LSquare extends SimpleToken
  case object RSquare extends SimpleToken
  case object LCurly extends SimpleToken
  case object RCurly extends SimpleToken

  case object ColonColon extends SimpleToken
  case object Backslash extends SimpleToken
  case object Arrow extends SimpleToken
  case object DotDot extends SimpleToken

  case object Eq extends SimpleToken

  case object Comma extends SimpleToken
  case object Semicolon extends SimpleToken
  case object Newline extends SimpleToken
  case object Eof extends SimpleToken
}
