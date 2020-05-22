package slang.parse

import slang.runtime.Value
import slang.util.TryObject._

sealed trait Stmt

object Stmt {

  case class Expression(expr: Expr) extends Stmt

  case class Let(pattern: Pattern, init: Expr) extends Stmt

  case class Print(expr: Expr) extends Stmt

  case class Match(patterns: List[Pattern], expr: Expr) extends Stmt {
    def hasHead(pred: Pattern => Boolean): Boolean = pred(patterns.head)
    def isElementalHead: Boolean = hasHead(h => h.tryAsInstanceOf[Pattern.Literal].exists(_.value.isElemental))
    def isLiteralValueHead(value: Value): Boolean = hasHead(h => h.tryAsInstanceOf[Pattern.Literal].exists(_.value == value))
    def isLiteralTypeHead(value: Value): Boolean = hasHead(h => h.tryAsInstanceOf[Pattern.Literal].exists(_.value.getType == value.getType))
    def isVariableHead: Boolean = hasHead(h => h.isInstanceOf[Pattern.Id] || h.isInstanceOf[Pattern.Ignore])
    def isListHead: Boolean = hasHead(h => h.isInstanceOf[Pattern.SlangList])
    def tailMatch: Match = Match(patterns.tail, expr)
  }

}
