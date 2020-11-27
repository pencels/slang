package slang.runtime

import slang.parse.Pattern
import slang.parse.Expr

sealed trait Value

object Value {
  case class Number(num: Double) extends Value
  case class Lambda(patterns: Seq[Pattern], result: Expr, env: Environment)
      extends Value
  case object Nothing extends Value
}
