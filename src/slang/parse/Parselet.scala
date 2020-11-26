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

sealed trait PrefixParselet {
  def parse(parser: Parser, op: Token): Expr
}

case object IdParselet extends PrefixParselet {
  override def parse(parser: Parser, op: Token): Expr = {
    op.ty match {
      case TokenType.Id(name)     => Expr(op.span, ExprType.Id(name))
      case TokenType.TypeId(name) => Expr(op.span, ExprType.TypeId(name))
      case _                      => throw new Exception("This shouldn't happen :)")
    }
  }
}

case object LiteralParselet extends PrefixParselet {
  override def parse(parser: Parser, op: Token): Expr = {
    op.ty match {
      case TokenType.Nothing => Expr(op.span, ExprType.Nothing)
      case _                 => ??? // TODO!
    }
  }
}

case object NumberParselet extends PrefixParselet {
  override def parse(parser: Parser, op: Token): Expr = {
    op.ty match {
      case TokenType.Number(num) => Expr(op.span, ExprType.Number(num))
      case _                     => throw new Exception("This shouldn't happen :)")
    }
  }
}

case object LetParselet extends PrefixParselet {
  override def parse(parser: Parser, op: Token): Expr = {
    val pattern = parser.pattern()
    parser.expect(TokenType.Eq, "Expected '=' after let pattern.")
    parser.skipNewlines() // Allow newlines after '='.
    val expr = parser.expr()
    Expr(op.span.merge(parser.prev.span), ExprType.Let(pattern, expr))
  }
}

case object ListParselet extends PrefixParselet {
  override def parse(parser: Parser, op: Token): Expr = {
    val elements = parser.parseDelimited(
      _.expr(),
      TokenType.Comma,
      TokenType.RSquare,
      "list expression",
      "element"
    )
    Expr(op.span.merge(parser.prev.span), ExprType.List(elements))
  }
}

case object GroupParselet extends PrefixParselet {
  override def parse(parser: Parser, op: Token): Expr = {
    val exprs = parser.parseDelimited(
      _.expr(),
      TokenType.Semicolon,
      TokenType.RParen,
      "parenthesized expression",
      "expression"
    )
    Expr(op.span.merge(parser.prev.span), ExprType.Group(exprs))
  }
}

sealed trait InfixParselet {
  def parse(parser: Parser, left: Expr, op: Token): Expr
  def getPrecedence: Int
}

case class BinOpParselet(val prec: Int, val isRight: Boolean)
    extends InfixParselet {
  def parse(parser: Parser, left: Expr, op: Token): Expr = {
    parser.skipNewlines() // Allow newlines after binary operators.
    val right = parser.expr(prec - (if (isRight) 1 else 0))
    Expr(left.span.merge(parser.prev.span), ExprType.BinOp(left, op, right))
  }
  def getPrecedence: Int = prec
}

case object AssignmentParselet extends InfixParselet {
  override def parse(parser: Parser, left: Expr, op: Token): Expr = {
    parser.skipNewlines()
    val expr = parser.expr()
    Expr(left.span.merge(parser.prev.span), ExprType.Assign(left, expr))
  }
  override def getPrecedence: Int = Precedence.ASSIGNMENT
}

case object CallParselet extends InfixParselet {
  override def parse(parser: Parser, left: Expr, op: Token): Expr = {
    val args = mutable.ListBuffer[Expr]()

    // There may not be any arguments.
    do {
      args.addOne(parser.expr(getPrecedence))
    } while (parser.currentPrecedence >= getPrecedence)

    Expr(left.span.merge(parser.prev.span), ExprType.Call(left, args.toSeq))
  }

  override def getPrecedence: Int = Precedence.CALL
}
