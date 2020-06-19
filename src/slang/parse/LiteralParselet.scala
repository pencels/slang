package slang.parse

import slang.lex.{Token, TokenType}
import slang.runtime.{Atom, Interpreter, Number, SlangNothing, SlangString, Value}

object LiteralParselet {
  def valueFromToken(token: Token): Value = {
    token.ty match {
      case TokenType.Nothing => SlangNothing
      case TokenType.Atom(name) => Atom(name)
      case TokenType.String(value) => SlangString(value)
      case TokenType.Number(value) => Number(value)
      case ty => throw new ParseException(token, s"Cannot derive value from non-literal token type ${ty}.")
    }
  }
}

class LiteralParselet extends PrefixParselet {
  override def parse(parser: Parser, token: Token) = new Expr.Literal(LiteralParselet.valueFromToken(token))
}