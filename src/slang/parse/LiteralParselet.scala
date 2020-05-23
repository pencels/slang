package slang.parse

import slang.lex.{Token, TokenType}
import slang.runtime.{Atom, Interpreter, Number, SlangNothing, SlangString, Value}

object LiteralParselet {
  def valueFromToken(token: Token): Value = {
    token.opType match {
      case TokenType.NOTHING => SlangNothing
      case TokenType.TRUE => Interpreter.TRUE_ATOM
      case TokenType.FALSE => Interpreter.FALSE_ATOM
      case TokenType.ATOM => Atom(token.value.asInstanceOf[String])
      case _ =>
        token.value match {
          case str: String => SlangString(str)
          case d: java.lang.Double => Number(d)
          case _ => throw new ParseException(token, "Cannot derive value for literal.")
        }
    }
  }
}

class LiteralParselet extends PrefixParselet {
  override def parse(parser: Parser, token: Token) = new Expr.Literal(LiteralParselet.valueFromToken(token))
}