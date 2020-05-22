package slang.runtime

import slang.parse.AbbreviatedAstPrinter
import slang.parse.Expr
import slang.parse.Stmt

case class DeferredMatchbox(matches: List[Stmt.Match], environment: Environment, deferredArgs: List[Value]) extends Value with Callable {
  val matchbox: Matchbox = Matchbox(matches, environment)

  def apply(interpreter: Interpreter, args: List[Value]): Value = {
    matchbox.apply(interpreter, deferredArgs ++ args)
  }

  override def toString: String = {
    var repr = environment.collapsedString
    repr += " deferring "
    repr += deferredArgs.mkString(" ")
    repr += " "
    repr += new AbbreviatedAstPrinter().print(Expr.Matchbox(matches))
    repr
  }

  override def getType: String = "Matchbox"

  override def asString: String = toString

  override def isElemental: Boolean = false
}