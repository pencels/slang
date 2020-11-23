package slang.parse

import slang.lex._

import scala.collection.mutable
import java.text.ParseException

object Precedence extends Enumeration {
  type Precedence = Int

  val SEQUENCE = 1
  val ASSIGNMENT = 2
  val CALL = 998
  val PREFIX = 999
  val POSTFIX = 1000
}

sealed trait Associativity
object Associativity {
  case object Left extends Associativity
  case object Right extends Associativity
  case object NonAssoc extends Associativity
}

trait PrefixParselet {
  def parse(parser: Parser, op: Token): Expr
}

object IdParselet extends PrefixParselet {
  override def parse(parser: Parser, op: Token): Expr = {
    op.ty match {
      case TokenType.Id(name)     => Expr.Id(name)
      case TokenType.TypeId(name) => Expr.TypeId(name)
      case _                      => throw new Exception("This shouldn't happen :)")
    }
  }
}

object NumberParselet extends PrefixParselet {
  override def parse(parser: Parser, op: Token): Expr = {
    op.ty match {
      case TokenType.Number(num) => Expr.Number(num)
      case _                     => throw new Exception("This shouldn't happen :)")
    }
  }
}

object LetParselet extends PrefixParselet {
  override def parse(parser: Parser, op: Token): Expr = {
    val pattern = parser.pattern()
    parser.expect(TokenType.Eq, "Expected '=' after let pattern.")
    parser.skipNewlines() // Allow newlines after '='.
    val expr = parser.expr()
    Expr.Let(pattern, expr)
  }
}

object ListParselet extends PrefixParselet {
  override def parse(parser: Parser, op: Token): Expr = {
    val elements = parser.parseDelimited(
      _.expr(),
      TokenType.Comma,
      TokenType.RSquare,
      "list expression",
      "element"
    )
    Expr.List(elements)
  }
}

object GroupParselet extends PrefixParselet {
  override def parse(parser: Parser, op: Token): Expr = {
    val exprs = parser.parseDelimited(
      _.expr(),
      TokenType.Semicolon,
      TokenType.RParen,
      "parenthesized expression",
      "expression"
    )
    Expr.Group(exprs)
  }
}

trait InfixParselet {
  def parse(parser: Parser, left: Expr, op: Token): Expr
  def getPrecedence: Int
}

object AssignmentParselet extends InfixParselet {
  override def parse(parser: Parser, left: Expr, op: Token): Expr = {
    parser.skipNewlines()
    val expr = parser.expr()
    Expr.Assign(left, expr)
  }
  override def getPrecedence: Int = Precedence.ASSIGNMENT
}

object CallParselet extends InfixParselet {
  override def parse(parser: Parser, left: Expr, op: Token): Expr = {
    val args = mutable.ListBuffer[Expr]()

    // There may not be any arguments.
    do {
      args.addOne(parser.expr(getPrecedence))
    } while (parser.currentPrecedence >= getPrecedence)

    Expr.Call(left, args.toSeq)
  }

  override def getPrecedence: Int = Precedence.CALL
}
