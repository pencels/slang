package slang.parse

import java.lang.{String => JString}
import slang.sourcemap.Span

case class Pattern(span: Span, ty: PatternType)

sealed trait PatternType
sealed trait IrrefutablePattern extends PatternType

object PatternType {
  case class Id(name: JString, binding: Option[Pattern] = None)
      extends IrrefutablePattern
  case class Ignore(binding: Option[Pattern] = None) extends IrrefutablePattern

  case object Nothing extends PatternType
  case class String(value: JString) extends PatternType
  case class Number(value: Double) extends PatternType
  case class List(patterns: Seq[Pattern]) extends PatternType

  case class Strict(pattern: IrrefutablePattern) extends PatternType
  case class Type(typename: JString) extends PatternType
  case class Constructor(name: JString, patterns: Seq[Pattern])
      extends PatternType
}
