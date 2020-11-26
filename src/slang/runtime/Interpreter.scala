package slang.runtime

import slang.parse.Expr
import slang.parse.ExprType
import slang.lex.TokenType
import slang.sourcemap.Span
import slang.ux.ErrorReporter

case class RuntimeError(span: Span, message: String) extends Exception

object Interpreter {
  def interpret(program: Seq[Expr], reporter: ErrorReporter): Unit = {
    val env = new Environment
    for (expr <- program) {
      try {
        println(eval(env, expr))
      } catch {
        case RuntimeError(span, message) =>
          reporter.error(span, message)
          return
      }
    }
  }

  def eval(env: Environment, expr: Expr): Value =
    expr.ty match {
      case ExprType.Nothing   => Value.Nothing
      case ExprType.Number(n) => Value.Number(n)
      case ExprType.BinOp(left, op, right) =>
        val TokenType.Op(name) = op.ty
        name match {
          case "+" =>
            val leftVal = castNumber(left.span, eval(env, left))
            val rightVal = castNumber(right.span, eval(env, right))
            Value.Number(leftVal + rightVal)
          case _ =>
            println(s"unsupported op: $name")
            Value.Nothing
        }
      case x =>
        print("couldn't eval: ")
        pprint.pprintln(x)
        Value.Nothing
    }

  def castNumber(span: Span, x: Value): Double = {
    x match {
      case Value.Number(num) => num
      case x                 => throw new RuntimeError(span, s"Expected Number but got $x")
    }
  }
}
