package slang.parse

import slang.lex._
import java.text.ParseException

class Parser(lexer: Lexer, operatorTrie: OperatorTrie) extends Iterator[Expr] {
  private[this] val tokenIterator = lexer.buffered
  private[this] var prevToken: Option[Token] = None

  skipNewlines() // Pump the lexer to skip any newlines that might exist.

  override def hasNext: Boolean = peek.isDefined

  override def next(): Expr = {
    val expr = topExpr()
    skipNewlines() // Get rid of any newlines after each Expr to maintain hasNext invariant.
    expr
  }

  def topExpr() = {
    ???
  }

  def skipNewlines() = {
    while (matchToken(TokenType.Newline)) {}
  }

  def matchToken(ty: SimpleToken) = {
    if (check(ty)) {
      advance()
      true
    } else {
      false
    }
  }

  def check(ty: SimpleToken) = peek.exists(_.ty == ty)

  def expect(ty: SimpleToken, message: String) = {
    if (check(ty)) {
      throw new ParserException(prevToken.get, message)
    }
    advance()
  }

  def peek = tokenIterator.headOption

  def advance() = {
    prevToken = tokenIterator.nextOption()
    prevToken.get
  }
}
