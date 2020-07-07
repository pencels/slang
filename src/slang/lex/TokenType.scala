package slang.lex

sealed trait TokenType

sealed trait PlainToken

object TokenType {
    case object Let extends TokenType with PlainToken
    case object Print extends TokenType with PlainToken
    case object KwOperator extends TokenType with PlainToken

    case object LParen extends TokenType with PlainToken
    case object RParen extends TokenType with PlainToken
    case object LCurly extends TokenType with PlainToken
    case object RCurly extends TokenType with PlainToken
    case object LBracket extends TokenType with PlainToken
    case object RBracket extends TokenType with PlainToken

    case object Semicolon extends TokenType with PlainToken
    case object Arrow extends TokenType with PlainToken
    case object Comma extends TokenType with PlainToken

    case object Eq extends TokenType with PlainToken

    case object Nothing extends TokenType with PlainToken
    case class Atom(name: java.lang.String) extends TokenType
    case class Operator(op: java.lang.String) extends TokenType
    case class Id(name: java.lang.String) extends TokenType
    case class Number(num: Double) extends TokenType
    case class String(value: java.lang.String) extends TokenType
    case class UnknownOperator(op: java.lang.String) extends TokenType

    case class Comment(text: java.lang.String) extends TokenType

    case object Newline extends TokenType with PlainToken
    case object Eof extends TokenType with PlainToken
    case class Error(message: java.lang.String) extends TokenType
}