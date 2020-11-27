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
import slang.runtime.Value.Lambda
import slang.parse.PatternType.Id
import slang.parse.PatternType.Ignore
import slang.parse.PatternType.Strict
import slang.parse.PatternType.Type
import slang.parse.PatternType.Constructor
import scala.annotation.tailrec

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
        case _ => pprint.pprintln(value)
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
      case ExprType.Group(inners) =>
        val localEnv = Environment.fresh(env)
        inners
          .map(eval(localEnv, _))
          .lastOption
          .getOrElse(Value.Nothing)
      case ExprType.Let(pat, expr) =>
        val value = eval(env, expr)
        assign(env, pat, value, define = true)(pat.span :: stack)
        Value.Nothing
      case ExprType.Assign(Expr(span, ExprType.Id(name)), expr) =>
        val value = eval(env, expr)
        env.set(name, value)(span :: stack)
        Value.Nothing
      case ExprType.BinOp(left, op, right) =>
        val TokenType.Op(name) = op.ty
        name match {
          case "+" =>
            val leftVal = castNumber(eval(env, left))(left.span :: stack)
            val rightVal = castNumber(eval(env, right))(right.span :: stack)
            Value.Number(leftVal + rightVal)
          case _ =>
            println(s"unsupported op: $name")
            Value.Nothing
        }
      case ExprType.Call(calleeExpr, argExprs) =>
        val callee = eval(env, calleeExpr)
        val args = argExprs.map(eval(env, _))
        call(callee, args)(calleeExpr.span :: stack)
      case ExprType.Lambda(patterns, result) =>
        Value.Lambda(patterns, result, env)
      case x =>
        print("couldn't eval: ")
        pprint.pprintln(x)
        Value.Nothing
    }

  @tailrec
  final def call(callee: Value, args: Seq[Value])(implicit
      stack: List[Span]
  ): Value = {
    val (value, rest) = callee match {
      case callee: Lambda => callLambda(callee, args)
      case _              => throw new RuntimeError(stack, "Value not callable.")
    }

    // If args leftover, call again with the result of the prefix call.
    // E.g. `f 1 2 3` should act like `(f 1 2) 3`.
    if (rest.nonEmpty) {
      call(value, rest)
    } else {
      value
    }
  }

  def callLambda(lambda: Value.Lambda, args: Seq[Value])(implicit
      stack: List[Span]
  ) = {
    val Lambda(patterns, result, env) = lambda
    val localEnv = Environment.fresh(env)
    for ((pat, value) <- patterns.zip(args)) {
      if (!assign(localEnv, pat, value, define = true)) {
        throw new RuntimeError(
          pat.span :: stack,
          s"Pattern $pat does not match value $value."
        )
      }
    }

    // Curry if fewer args than patterns.
    val value = if (args.size < patterns.size) {
      Value.Lambda(patterns.drop(args.size), result, localEnv)
    } else {
      eval(localEnv, result)
    }

    (value, args.drop(patterns.size))
  }

  /**
    * Assigns a value to a pattern.
    *
    * @param env the env to assign the value under
    * @param pat the pattern on the LHS of the assignment
    * @param value the value on the RHS of the assignment
    * @param define does the value create a new variable in the environment? Defaults to `false`.
    * @param stack span stack for traces
    * @return `true` if the pattern matches, `false` otherwise.
    */
  def assign(
      env: Environment,
      pat: Pattern,
      value: Value,
      define: Boolean = false
  )(implicit
      stack: List[Span]
  ): Boolean = {
    pat.ty match {
      case Id(name, Some(innerPat)) =>
        if (assign(env, innerPat, value)) {
          if (define) {
            env.define(name, value)(pat.span :: stack)
          } else {
            env.set(name, value)(pat.span :: stack)
          }
          true
        } else {
          false
        }
      case Id(name, _) =>
        if (define) {
          env.define(name, value)(pat.span :: stack)
        } else {
          env.set(name, value)(pat.span :: stack)
        }
        true
      case Ignore(Some(innerPat)) => assign(env, innerPat, value)
      case Ignore(_)              => true
      case PatternType.Nothing    => value == Value.Nothing
      case PatternType.String(_)  => false
      case PatternType.Number(num) =>
        value match {
          case Value.Number(valueNum) => num == valueNum
          case _                      => false
        }
      case PatternType.List(patterns)  => false
      case Strict(pattern)             => false
      case Type(typename)              => false
      case Constructor(name, patterns) => false
    }
  }

  def castNumber(x: Value)(implicit stack: List[Span]): Double = {
    x match {
      case Value.Number(num) => num
      case x =>
        throw new RuntimeError(stack, s"Expected Number but got $x")
    }
  }
}
