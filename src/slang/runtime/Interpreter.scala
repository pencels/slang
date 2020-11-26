package slang.runtime

import slang.parse.Expr
import slang.parse.ExprType
import slang.lex.TokenType
import slang.sourcemap.Span
import slang.ux.ErrorReporter

case class RuntimeError(stack: List[Span], message: String) extends Exception

object Interpreter {
  def interpret(program: Seq[Expr], reporter: ErrorReporter): Unit = {
    val env = new Environment
    var value: Value = Value.Nothing
    for (expr <- program) {
      try {
        value = eval(env, expr)
      } catch {
        case RuntimeError(spans, message) =>
          reporter.trace(spans, message)
          return
      }
    }

    println(value)
  }

  def eval(env: Environment, expr: Expr)(implicit
      stack: List[Span] = Nil
  ): Value =
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

  def castNumber(span: Span, x: Value)(implicit stack: List[Span]): Double = {
    x match {
      case Value.Number(num) => num
      case x =>
        throw new RuntimeError(span :: stack, s"Expected Number but got $x")
    }
  }
}
