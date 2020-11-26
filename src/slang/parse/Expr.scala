package slang.parse

import java.lang.{String => JString}

import slang.lex._
import slang.sourcemap._

case class Expr(span: Span, ty: ExprType)

sealed trait ExprType
object ExprType {
  case class Id(name: JString) extends ExprType
  case class TypeId(name: JString) extends ExprType
  case object Nothing extends ExprType

  case class Let(pattern: Pattern, expr: Expr) extends ExprType
  case class Assign(left: Expr, right: Expr) extends ExprType

  case class BinOp(left: Expr, op: Token, right: Expr) extends ExprType
  case class Call(callee: Expr, args: Seq[Expr]) extends ExprType

  object String {
    def from(segments: Seq[Either[Expr, JString]]) =
      segments match {
        case Seq(Right(str)) => String(str)
        case _               => InterpolatedString(segments)
      }
  }
  case class String(value: JString) extends ExprType
  case class InterpolatedString(segments: Seq[Either[Expr, JString]])
      extends ExprType
  case class Number(value: Double) extends ExprType
  case class List(exprs: Seq[Expr]) extends ExprType

  case class Sequence(exprs: Seq[Expr]) extends ExprType
  case class Lazy(exprs: Seq[Expr]) extends ExprType
  case class Group(exprs: Seq[Expr]) extends ExprType

  case class Fn(name: Expr, patterns: Seq[Pattern], result: Expr)
      extends ExprType
  case class Lambda(patterns: Seq[Pattern], result: Seq[Expr]) extends ExprType
  case class Matchbox(rows: Seq[MatchboxRow]) extends ExprType
  case class MatchboxRow(patterns: Seq[Pattern], result: Seq[Expr])
      extends ExprType

  case class RegisterOperator(
      fixity: Fixity,
      operator: Expr,
      constraints: Seq[PrecedenceConstraint]
  ) extends ExprType
}

trait Fixity
object Fixity {
  case object Prefix extends Fixity
  case object Infix extends Fixity
  case object Postfix extends Fixity
}

trait PrecedenceConstraint
object PrecedenceConstraint {
  case class With(op: ExprType.Id) extends PrecedenceConstraint
  case class Above(op: ExprType.Id) extends PrecedenceConstraint
  case class Below(op: ExprType.Id) extends PrecedenceConstraint
}
