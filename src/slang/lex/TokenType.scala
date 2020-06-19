package slang.lex

sealed trait TokenType

sealed trait PlainToken

object TokenType {
    case object Let extends TokenType with PlainToken
    case object Print extends TokenType with PlainToken

    case object LParen extends TokenType with PlainToken
    case object RParen extends TokenType with PlainToken
    case object LCurly extends TokenType with PlainToken
    case object RCurly extends TokenType with PlainToken
    case object LBracket extends TokenType with PlainToken
    case object RBracket extends TokenType with PlainToken

    case object Semicolon extends TokenType with PlainToken
    case object Arrow extends TokenType with PlainToken
    case object At extends TokenType with PlainToken
    case object Bang extends TokenType with PlainToken
    case object Comma extends TokenType with PlainToken
    case object Dot extends TokenType with PlainToken
    case object DotDot extends TokenType with PlainToken

    case object Eq extends TokenType with PlainToken
    case object EqEq extends TokenType with PlainToken
    case object Ne extends TokenType with PlainToken
    case object Lt extends TokenType with PlainToken
    case object Le extends TokenType with PlainToken
    case object Gt extends TokenType with PlainToken
    case object Ge extends TokenType with PlainToken

    case object Plus extends TokenType with PlainToken
    case object Minus extends TokenType with PlainToken
    case object Star extends TokenType with PlainToken
    case object Slash extends TokenType with PlainToken
    case object Percent extends TokenType with PlainToken

    case object Ampersand extends TokenType with PlainToken
    case object Pipe extends TokenType with PlainToken
    case object AmpersandBang extends TokenType with PlainToken
    case object StarBang extends TokenType with PlainToken

    case object Nothing extends TokenType with PlainToken
    case class Atom(name: java.lang.String) extends TokenType
    case class Id(name: java.lang.String) extends TokenType
    case class Number(num: Double) extends TokenType
    case class String(value: java.lang.String) extends TokenType

    case class Comment(text: java.lang.String) extends TokenType

    case object Newline extends TokenType with PlainToken
    case object Eof extends TokenType with PlainToken
    case class Error(message: java.lang.String) extends TokenType
}