package slang.parse

import java.lang.{String => JString}

sealed trait Expr

object Expr {
  case class Id(name: JString) extends Expr
  case class TypeId(name: JString) extends Expr
  case object Nothing extends Expr

  case class Let(pattern: Pattern, expr: Expr) extends Expr

  case class Call(callee: Expr, args: Seq[Expr]) extends Expr

  object String {
    def from(segments: Seq[Either[Expr, JString]]) =
      segments match {
        case Seq(Right(str)) => String(str)
        case _               => InterpolatedString(segments)
      }
  }
  case class String(value: JString) extends Expr
  case class InterpolatedString(segments: Seq[Either[Expr, JString]])
      extends Expr
  case class Number(value: Double) extends Expr
  case class List(exprs: Seq[Expr]) extends Expr

  case class Sequence(exprs: Seq[Expr]) extends Expr
  case class Lazy(exprs: Seq[Expr]) extends Expr

  case class Fn(name: Expr.Id, patterns: Seq[Pattern], result: Expr)
      extends Expr
  case class Lambda(patterns: Seq[Pattern], result: Expr) extends Expr
  case class Matchbox(rows: Seq[MatchboxRow]) extends Expr
  case class MatchboxRow(patterns: Seq[Pattern], result: Seq[Expr]) extends Expr

  case class RegisterOperator(
      fixity: Fixity,
      operator: Expr.Id,
      constraints: Seq[PrecedenceConstraint]
  ) extends Expr
}

trait Fixity
object Fixity {
  case object Prefix extends Fixity
  case object Infix extends Fixity
  case object Postfix extends Fixity
}

trait PrecedenceConstraint
object PrecedenceConstraint {
  case class With(op: Expr.Id) extends PrecedenceConstraint
  case class Above(op: Expr.Id) extends PrecedenceConstraint
  case class Below(op: Expr.Id) extends PrecedenceConstraint
}
