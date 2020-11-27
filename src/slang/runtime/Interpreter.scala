package slang.runtime

import scala.io.AnsiColor._

import slang.parse.Expr
import slang.parse.ExprType
import slang.lex.TokenType
import slang.sourcemap.Span
import slang.ux.ErrorReporter
import slang.sourcemap.SourceMap
import slang.sourcemap.Loc
import slang.parse.Pattern
import slang.parse.PatternType

case class RuntimeError(stack: List[Span], message: String) extends Exception

object Interpreter {
  val env = new Environment

  def interpret(program: Seq[Expr], sourceMap: SourceMap): Unit = {
    var value: Value = Value.Nothing
    for (expr <- program) {
      try {
        value = eval(env, expr)
      } catch {
        case RuntimeError(spans, message) =>
          trace(sourceMap, spans, message)
          return
      }
    }

    if (value != Value.Nothing) {
      value match {
        case Value.Number(num) =>
          var numStr = num.toString
          if (numStr.endsWith(".0")) {
            numStr = numStr.slice(0, numStr.length - 2)
          }
          println(numStr)
        case _ => println(value)
      }
    }
  }

  def trace(sourceMap: SourceMap, spans: Seq[Span], message: String) = {
    System.err.println(s"$BOLD${RED}Runtime Error:$RESET$BOLD $message$RESET")
    for (span <- spans) {
      val (file, Loc(line, col)) = sourceMap.location(span.start)
      val lineStr = file.getSourceAtLine(line).stripLineEnd

      System.err.println(s"$RED -> at ${file.path}:$line:$col$RESET")
      System.err.println(s"      $lineStr")
      val preHighlightStr = lineStr.slice(0, col - 1)
      val indent = preHighlightStr.replaceAll("[^\t]", " ")
      System.err.println(s"      " + indent + s"$RED^$RESET")
    }
  }

  def eval(env: Environment, expr: Expr)(implicit
      stack: List[Span] = Nil
  ): Value =
    expr.ty match {
      case ExprType.Nothing   => Value.Nothing
      case ExprType.Number(n) => Value.Number(n)
      case ExprType.Id(name)  => env.get(name)(expr.span :: stack)
      case ExprType.Let(Pattern(span, PatternType.Id(name, _)), expr) =>
        val value = eval(env, expr)
        env.define(name, value)(span :: stack)
        Value.Nothing
      case ExprType.Assign(Expr(span, ExprType.Id(name)), expr) =>
        val value = eval(env, expr)
        env.set(name, value)(span :: stack)
        Value.Nothing
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
