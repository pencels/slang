package slang.parse

import slang.lex.Token
import slang.parse.Stmt.Match
import slang.runtime.Value

sealed trait Expr

object Expr {

  case class Assign(left: Pattern, right: Expr) extends Expr

  case class Binary(left: Expr, op: Token, right: Expr) extends Expr

  case class Block(statements: List[Stmt]) extends Expr

  case class Matchbox(matches: List[Match]) extends Expr

  case class Call(left: Expr, args: List[Expr]) extends Expr

  case class Grouping(inner: Expr) extends Expr

  case class Id(name: String) extends Expr

  case class Literal(value: Value) extends Expr

  case class Postfix(expr: Expr, op: Token) extends Expr

  case class SlangList(elements: List[Expr]) extends Expr

  case class Prefix(op: Token, expr: Expr) extends Expr

}



