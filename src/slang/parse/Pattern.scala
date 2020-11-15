package slang.parse

import java.lang.{String => JString}

import slang.parse.Expr

sealed trait Pattern
sealed trait IrrefutablePattern extends Pattern

object Pattern {
  case class Id(name: JString, binding: Option[Pattern] = None)
      extends Pattern
      with IrrefutablePattern
  case class Ignore(binding: Option[Pattern] = None)
      extends Pattern
      with IrrefutablePattern

  case object Nothing extends Pattern
  case class String(value: JString) extends Pattern
  case class Number(value: Double) extends Pattern
  case class List(patterns: Seq[Pattern]) extends Pattern

  case class Strict(pattern: IrrefutablePattern) extends Pattern
  case class Type(typename: JString) extends Pattern
  case class Constructor(name: JString, patterns: Seq[Pattern]) extends Pattern

  def fromLiteral(expr: Expr) =
    expr match {
      case Expr.Nothing       => Pattern.Nothing
      case Expr.String(value) => Pattern.String(value)
      case Expr.Number(value) => Pattern.Number(value)
      case _                  => throw new Exception(s"Can't get pattern from $expr")
    }
}
