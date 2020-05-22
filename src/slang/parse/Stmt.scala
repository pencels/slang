package slang.parse

import slang.runtime.Value
import slang.util.TryObject._

sealed trait Stmt

object Stmt {

  case class Expression(expr: Expr) extends Stmt

  case class Let(pattern: Pattern, init: Expr) extends Stmt

  case class Print(expr: Expr) extends Stmt

  case class Match(patterns: List[Pattern], expr: Expr) extends Stmt

}
