package slang.parse

import slang.lex._

import scala.collection.mutable
import scala.annotation.tailrec
import slang.lex.TokenType.Eof

case class ParserException(token: Token, message: String)
    extends Exception(message)

case class ParserError(span: Span, message: String)

class Parser(lexer: Lexer, context: ParseContext)
    extends Iterator[Option[Expr]] {
  private[this] val tokenIterator = lexer.buffered
  private[this] var prevToken: Option[Token] = None

  override def hasNext: Boolean = !isAtEof

  override def next(): Option[Expr] = {
    try {
      skipNewlines()
      val expr = topExpr()
      Some(expr)
    } catch {
      case ParserException(tok, message) =>
        synchronize()
        context.reporter.error(tok.span, message)
        None
    }
  }

  def topExpr() = {
    expr()
  }

  /**
    * Advance token stream until reaching a synchronization point,
    * i.e. a place where the parser can reasonably recover from
    * a parsing error.
    */
  @tailrec
  final def synchronize(): Unit = {
    advance()
    peek.ty match {
      case TokenType.Newline => skipNewlines()
      case TokenType.Eof     =>
      case _                 => synchronize()
    }
  }

  def getPrefixParselet(token: Token): PrefixParselet = {
    token.ty match {
      case TokenType.Let       => LetParselet
      case TokenType.Id(_)     => IdParselet
      case TokenType.TypeId(_) => IdParselet
      case TokenType.Number(_) => NumberParselet
      case TokenType.LParen    => GroupParselet
      case TokenType.LSquare   => ListParselet

      case TokenType.Op(op) =>
        throw new ParserException(
          token,
          s"Operator '$op' was not defined and cannot be parsed."
        )

      // Give the programmer a nudge if the token is unexpected.
      case TokenType.RParen =>
        throw new ParserException(
          token,
          s"Unexpected ')'. Might have too many closing parens here."
        )
      case TokenType.RSquare =>
        throw new ParserException(
          token,
          s"Unexpected ']'. Might have too many closing brackets here."
        )
      case TokenType.RCurly =>
        throw new ParserException(
          token,
          s"Unexpected '}'. Might have too many closing curly braces here."
        )
      case _ =>
        throw new ParserException(
          token,
          s"Could not parse token $token as prefix."
        )
    }
  }

  def getInfixParselet(token: Token): Option[InfixParselet] = {
    token.ty match {
      case TokenType.Eq => Some(AssignmentParselet)
      case TokenType.LParen | TokenType.LCurly | TokenType.LSquare |
          TokenType.Nothing | _: TokenType.Id | _: TokenType.Atom |
          _: TokenType.Number | _: TokenType.String =>
        Some(CallParselet)

      case TokenType.RSquare =>
        throw new ParserException(
          token,
          s"Unexpected ']'. Might have too many closing brackets here."
        )

      case TokenType.Op(op) =>
        throw new ParserException(
          token,
          s"Operator '$op' was not defined and cannot be parsed."
        )

      case _ => None
    }
  }

  def currentPrecedence: Int = {
    getInfixParselet(peek) match {
      case Some(parselet) => parselet.getPrecedence
      case None           => 0
    }
  }

  def expr(prec: Int = 0) = {
    val token = advance()
    val prefix = getPrefixParselet(token)
    var left = prefix.parse(this, token)
    parseInfix(left, token, prec)
  }

  @tailrec
  final def parseInfix(
      left: Expr,
      infixToken: Token,
      prec: Int
  ): Expr = {
    var token = infixToken
    // If there is a non-infix operator token, skip infix operator parsing.
    getInfixParselet(peek) match {
      case Some(infix) if prec < infix.getPrecedence =>
        // Don't eat a token for call parselet, since it needs to preserve
        // tokens for expression() calls.
        if (infix != CallParselet) {
          token = advance()
        }
        val newLeft = infix.parse(this, left, token)
        parseInfix(newLeft, token, prec)
      case _ => left
    }
  }

  def pattern(): Pattern = {
    peek.ty match {
      case TokenType.Id(name) =>
        advance()
        Pattern(prev.span, PatternType.Id(name, None))
      case x => throw new ParserException(peek, "Unsupported pattern.")
    }
  }

  @tailrec
  final def skipNewlines(): Unit = {
    peek.ty match {
      case TokenType.Newline =>
        advance()
        skipNewlines()
      case _ =>
    }
  }

  def parseDelimited[T](
      elementParser: Parser => T,
      delim: SimpleToken,
      end: SimpleToken,
      constructName: String,
      elementName: String
  ) = {
    val inners = new mutable.ListBuffer[T]()

    while (!check(end)) {
      skipNewlines()

      if (isAtEof) {
        throw new ParserException(
          peek,
          s"Unexpected end of file while parsing $constructName."
        )
      }

      if (!check(end)) {
        inners.addOne(elementParser(this))
      }

      peek.ty match {
        case ty if ty == end   =>
        case ty if ty == delim => advance()
        case TokenType.Newline => advance()
        case ty =>
          throw new ParserException(
            peek,
            s"Expected $delim or $end but got $ty."
          )
      }
    }
    expect(end, s"Expected $end.")

    inners.toList
  }

  def matchToken(ty: SimpleToken) = {
    if (check(ty)) {
      advance()
      true
    } else {
      false
    }
  }

  def check(ty: SimpleToken) = peek.ty == ty

  def expect(ty: SimpleToken, message: String) = {
    if (!check(ty)) {
      throw new ParserException(peek, message)
    }

    advance()
  }

  def expectId(message: String): Token = {
    peek.ty match {
      case _: TokenType.Id => advance()
      case _               => throw new ParserException(peek, message)
    }
  }

  def isAtEof = peek.ty == TokenType.Eof

  def peek = tokenIterator.head

  def prev = prevToken.get

  def advance() = {
    val token = tokenIterator.next()
    prevToken = Some(token)
    token
  }
}
