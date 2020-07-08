package slang.parse

import slang.lex.{Token, TokenType}
import slang.runtime._

object LiteralParselet {
  def valueFromToken(token: Token): Value = {
    token.ty match {
      case TokenType.Nothing => Value.Nothing
      case TokenType.Atom(name) => Value.Atom(name)
      case TokenType.String(value) => Value.String(value)
      case TokenType.Number(value) => Value.Number(value)
      case ty => throw new ParseException(token, s"Cannot derive value from non-literal token type ${ty}.")
    }
  }
}

class LiteralParselet extends PrefixParselet {
  override def parse(parser: Parser, token: Token) = new Expr.Literal(LiteralParselet.valueFromToken(token))
}