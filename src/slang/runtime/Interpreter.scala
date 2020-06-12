package slang.runtime

import slang.lex.TokenType
import slang.parse.{Expr, Pattern}

import scala.annotation.tailrec
import slang.lex.TokenType._

object Interpreter {
  val TRUE_ATOM: Value = Atom("true")
  val FALSE_ATOM: Value = Atom("false")
}

class Interpreter {
  def interpret(env: Environment, exprs: List[Expr]): Value = {
    var ret: Value = SlangNothing

    for (expr <- exprs) {
      ret = eval(env, expr)
    }

    ret
  }

  /** Convenience for strictCoerce(eval(env, exp)) */
  def strictEval(env: Environment, expr: Expr, full: Boolean = false): Value = strictCoerce(eval(env, expr), full)

  /** Evaluate a lazy value recursively until reaching a non-lazy value. */
  @tailrec
  final def strictCoerce(value: Value, full: Boolean = false): Value  = {
    value match {
      case Lazy(env, expr) =>
        val value = eval(new Environment(env), expr)
        if (full) strictCoerce(value, full) else value
      case _ => value
    }
  }

  /** Evaluate a thunk explicitly, or uses the cached value if it has been evaluated once. */
  def strictCoerceThunk(thunk: Thunk, full: Boolean = false): Value = thunk match {
    case Thunk(Some(value), _) => value
    case Thunk(_, unevaluatedValue) => {
      val value = strictCoerce(unevaluatedValue, full)
      thunk.cachedValue = Some(value)
      value
    }
  }

  def eval(env: Environment, expr: Expr): Value = expr match {
    case Expr.Assign(left, right) =>
      val value = eval(env, right)
      // NOTE(michael): I know, wrapping this in a thunk is kinda ugly here, but it's for a good cause.
      if (!assign(env, left, Thunk.from(value), define = false)) throw new RuntimeError(null, "Match failed.")
      value
    case bin: Expr.Binary => evalBinExpr(env, bin)
    case Expr.Call(callee, argExprs) =>
      val closure = eval(env, callee)
      val args = argExprs.map(eval(env, _))
      call(env, closure, args)
    case Expr.Grouping(inner) => eval(env, inner)
    case Expr.Id(name) => env.get(name)
    case Expr.Literal(value) => value
    case post: Expr.Postfix => evalPostfixExpr(env, post)
    case Expr.SlangList(exprs) => SlangList(exprs.map(eval(env, _)))
    case unary: Expr.Prefix => evalPrefixOperator(env, unary)
    case Expr.Block(expr) => Lazy(env, expr)
    case Expr.Matchbox(matches) => Matchbox.toMatchboxOrHashbox(env, matches)
    case Expr.Let(pattern, init) => {
      val value = eval(env, init)

      // TODO(michael): If we get types, we can verify the pattern is irrefutable first.
      if (!assign(env, pattern, Thunk.from(value))) {
        throw new FailedMatchException(pattern, value)
      }

      SlangNothing
    }
    case Expr.Print(expr) => println(eval(env, expr).toSlangString); SlangNothing
    case Expr.MatchRow(_, _) => ??? // Should never reach this ???
    case Expr.Seq(exprs) => {
      var value: Value = SlangNothing
      for (expr <- exprs) {
        value = eval(env, expr)
      }
      value
    }
  }

  def evalBinExpr(env: Environment, expr: Expr.Binary): Value = {
    val op = expr.op

    // TODO(chris): Write a compiler pass or parselet which transforms binary ';' exprs into Expr.Seq.
    if (op.opType == TokenType.SEMI) {
      eval(env, expr.left)
      return eval(env, expr.right)
    }

    val left = strictEval(env, expr.left, full = true)
    val right = strictEval(env, expr.right, full = true)

    op.opType match {
      case TokenType.AT => call(env, right, List(left))
      case TokenType.PLUS =>
        (left, right) match {
          // We love overloaded operators
          case (SlangString(value), _) => SlangString(value + right.toSlangString)
          case (SlangList(leftValues), SlangList(rightValues)) => SlangList(leftValues ++ rightValues)
          case (Matchbox(left), Matchbox(right)) =>
            Matchbox(left ++ right)
          case (Matchbox(left), r: Hashbox) =>
            val Matchbox(right) = transmuteHashbox(r)
            Matchbox(left ++ right)
          case (l: Hashbox, r: Hashbox) =>
            val Matchbox(left) = transmuteHashbox(l)
            val Matchbox(right) = transmuteHashbox(r)
            Matchbox(left ++ right)
          case (l: Hashbox, Matchbox(right)) =>
            val Matchbox(left) = transmuteHashbox(l)
            Matchbox(left ++ right)
          case _ =>
            Number(left.tryAsDouble(op) + right.tryAsDouble(op))
        }
      case TokenType.MINUS =>
        Number(left.tryAsDouble(op) - right.tryAsDouble(op))
      case TokenType.STAR =>
        Number(left.tryAsDouble(op) * right.tryAsDouble(op))
      case TokenType.SLASH =>
        Number(left.tryAsDouble(op) / right.tryAsDouble(op))
      case TokenType.PERCENT => 
        Number(left.tryAsDouble(op) % right.tryAsDouble(op))
      case TokenType.EQEQ =>
        if (left == right) Interpreter.TRUE_ATOM
        else Interpreter.FALSE_ATOM
      case TokenType.NE =>
        if (left == right) Interpreter.FALSE_ATOM
        else Interpreter.TRUE_ATOM
      case TokenType.LT =>
        if (left.tryAsDouble(op) < right.tryAsDouble(op)) Interpreter.TRUE_ATOM
        else Interpreter.FALSE_ATOM
      case TokenType.LE =>
        if (left.tryAsDouble(op) <= right.tryAsDouble(op)) Interpreter.TRUE_ATOM
        else Interpreter.FALSE_ATOM
      case TokenType.GT =>
        if (left.tryAsDouble(op) > right.tryAsDouble(op)) Interpreter.TRUE_ATOM
        else Interpreter.FALSE_ATOM
      case TokenType.GE =>
        if (left.tryAsDouble(op) >= right.tryAsDouble(op)) Interpreter.TRUE_ATOM
        else Interpreter.FALSE_ATOM
      case _ => throw new RuntimeError(expr.op, s"Unexpected operator: ${expr.op.opType}")
    }
  }

  def evalPostfixExpr(env: Environment, expr: Expr.Postfix): Value = {
    val value = eval(env, expr.expr)
    expr.op.opType match {
      case TokenType.BANG =>
        var i = value.tryAsDouble(expr.op).intValue
        var n = 1

        while (i > 0) {
          n *= i
          i -= 1
        }
        Number(n)
      case _ => throw new RuntimeError(expr.op, s"Unexpected operator: ${expr.op.opType}")
    }
  }

  def evalPrefixOperator(env: Environment, expr: Expr.Prefix): Value = {
    val innerExpr = expr.expr
    expr.op.opType match {
      case MINUS => {
        val value = strictEval(env, innerExpr, full = true).tryAsDouble(expr.op)
        Number(-value)
      }
      case PLUS => {
        val value = strictEval(env, innerExpr, full = true).tryAsDouble(expr.op)
        Number(value)
      }
      case AMPERSAND => Lazy(env, innerExpr)
      case STAR => strictEval(env, innerExpr)
      case STAR_BANG => strictEval(env, innerExpr, full = true)
      case _ => ???
    }
  }

  @tailrec
  final def call(env: Environment, callee: Value, args: List[Value]): Value = {
    strictCoerce(callee, full = true) match {
      case matchbox: Matchbox => {
        applyMatchbox(matchbox, args) match {
          case (value, Nil) => value
          case (value, rest) => call(env, value, rest)
        }
      }
      case hashbox: Hashbox => {
        applyHashbox(hashbox, args) match {
          case (value, Nil) => value
          case (value, rest) => call(env, value, rest)
        }
      }
      case callee => 
        // Use __primitives__ to handle method calls on primitives.
        val typeAtom = Atom(callee.getType)
        val primitives = env.get("__primitives__");
        val newArgs = typeAtom :: Atom("get") :: callee :: args;
        call(env, primitives, newArgs)
    }
  }

  def assign(env: Environment, pattern: Pattern, arg: Thunk, define: Boolean = true): Boolean = {
    pattern match {
      case Pattern.Id(id) =>
        if (define)
          env.define(id.lexeme, arg.unevaluatedValue)
        else
          env.set(id.lexeme, arg.unevaluatedValue)
        true
      case _: Pattern.Ignore => true
      case Pattern.Strict(inner, full) => assign(env, inner, Thunk.fromStrict(strictCoerceThunk(arg, full)), define)
      case Pattern.Literal(value) => value == strictCoerceThunk(arg, full = true)
      case Pattern.SlangList(patterns) =>
        strictCoerceThunk(arg, full = true) match {
          case SlangList(values) => assignList(env, patterns, values, define)
          case _ => false
        }
      case Pattern.Spread(_) => throw new RuntimeError(null, "Unexpected spread pattern, not at end of list!")
    }
  }

  def assignList(env: Environment, patterns: List[Pattern], values: List[Value], define: Boolean = true): Boolean = (patterns, values) match {
    case (Nil, Nil) =>
      // We're at the end of the list... (or matching empty lists)
      true
    case (List(Pattern.Spread(spread)), values) =>
      if (define)
        env.define(spread.lexeme, SlangList(values))
      else
        env.set(spread.lexeme, SlangList(values))
      // Match the rest of the list with a spread
      true
    case (pattern :: patterns, value :: values) =>
      // Do a 1:1 match of a list pattern and a value pattern
      assign(env, pattern, Thunk.from(value), define) && assignList(env, patterns, values, define)
    case _ =>
      // Either patterns is exhausted, or values is exhausted.
      false
  }

  /** Apply a matchbox to a list of arguments.
   *
   * Given the arity N of the matchbox, and M parameters, this will return:
   * if N < M -> Applied matchbox, plus (M - N) leftover unapplied arguments,
   * if N = M -> Applied matchbox, and an empty list,
   * if N > M -> Partially applied matchbox, and and empty list.
   */
  def applyMatchbox(matchbox: Matchbox, values: List[Value]): (Value, List[Value]) = {
    assert(values.nonEmpty, "We must have at least one value to apply to this matchbox...")

    // NOTE: we need to "refresh"/clone the environments for the matchbox, because this matchbox
    // could be cloned and we would risk leaking a value out of our scope!
    // TODO(michael): We could do withNewEnvironment heuristically, e.g. if our patterns have a variable.
    var remainingRows = matchbox.rows map {
      _.withNewEnvironment
    }
    var remainingValues = values

    while (remainingValues.nonEmpty) {
      // Chop off one arg, and represent that arg as a thonk
      val arg = Thunk.from(remainingValues.head)
      remainingValues = remainingValues.tail

      // Build the list of new rows... but backwards.
      var newRows: List[MatchboxRow] = Nil

      for (MatchboxRow(innerEnvironment, parameters, result) <- remainingRows) {
        // I think we can assume every matchbox has at least one parameter.
        val parameter = parameters.head
        val remainingParameters = parameters.tail

        // Try to assign the pattern, mutating the environment if needed.
        if (assign(innerEnvironment, parameter, arg)) {
          // If the row is empty, then we're done here.
          if (remainingParameters.isEmpty) {
            return (eval(innerEnvironment, result), remainingValues)
          }

          // Otherwise, we'll just save this pattern for later!
          newRows = MatchboxRow(innerEnvironment, remainingParameters, result) :: newRows
        }
      }

      if (newRows.isEmpty) {
        throw new RuntimeError(null, "Matchbox is exhausted with no match")
      }

      // This is an artifact of having to build up a new list...
      remainingRows = newRows.reverse
    }

    (Matchbox(remainingRows), remainingValues)
  }

  /** Apply a hashbox to a list of arguments.
   *
   * Given the arity N of the hashbox, and M parameters, this will return:
   * if N < M -> Applied hashbox, plus (M - N) leftover unapplied arguments,
   * if N = M -> Applied hashbox, and an empty list,
   * if N > M -> Deferred hashbox (saving the args for later), and an empty list.
   */
  def applyHashbox(hashbox: Hashbox, args: List[Value]): (Value, List[Value]) = {
    val Hashbox(partialArguments, innerEnvironment, arity, rows, extraRow) = hashbox

    (hashbox.partialArguments ++ args) splitAt hashbox.arity match {
      case (values, remainder) if values.length == hashbox.arity => {
        // We NEED to coerce these values to hash them correctly.
        val strictValues = values.map(strictCoerce(_, full = true))

        if (rows.contains(strictValues)) {
          // Cool, we just hash our value and call it a day.
          val expr = hashbox.rows(strictValues)
          (eval(new Environment(hashbox.innerEnvironment), expr), remainder)
        } else extraRow match {
          // Otherwise, defer to the extra matchbox row...
          case Some(HashboxRow(parameters, result)) =>
            val allValues = strictValues ++ remainder
            applyFinalHashboxRow(new Environment(innerEnvironment), parameters, result, allValues)
          case None =>
            throw new RuntimeError(null, "Matchbox is exhausted with no match")
        }
      }
      case (partialArguments, Nil) if args.length < hashbox.arity => {
        // If we have less than the needed values, put her back together. Defer!
        // TODO(michael): Semantically, it might be useful to coerce these args of their lazy NOW instead of later?
        (Hashbox(partialArguments, innerEnvironment, arity, rows, extraRow), Nil)
      }
    }
  }

  // This is just a pared-down version of applyMatchbox, simplifying for a single row.
  def applyFinalHashboxRow(innerEnvironment: Environment, parameters: List[Pattern], result: Expr, values: List[Value]): (Value, List[Value]) = {
    var remainingParameters = parameters
    var remainingValues = values

    while (remainingValues.nonEmpty) {
      // Chop off one arg, and represent that arg as a thonk
      val arg = Thunk.from(remainingValues.head)
      remainingValues = remainingValues.tail

      // I think we can assume every matchbox has at least one parameter.
      val parameter = remainingParameters.head
      remainingParameters = remainingParameters.tail

      // Try to assign the pattern, mutating the environment if needed.
      if (assign(innerEnvironment, parameter, arg)) {
        // If the row is empty, then we're done here.
        if (remainingParameters.isEmpty) {
          return (eval(innerEnvironment, result), remainingValues)
        }

        // Otherwise, just continue...
      } else {
        throw new RuntimeError(null, "Matchbox is exhausted with no match")
      }
    }

    (Matchbox(List(MatchboxRow(innerEnvironment, remainingParameters, result))), remainingValues)
  }

  def transmuteHashbox(hashbox: Hashbox): Matchbox = {
    val Hashbox(partialArguments, env, _, rows, extraRow) = hashbox
    var matchboxRows = rows.map({case (k, v) => MatchboxRow(env, k map { Pattern.Literal(_) }, v)}).toList

    // Append the extra row, if exists.
    extraRow match {
      case Some(HashboxRow(parameters, result)) =>
        matchboxRows ++= List(MatchboxRow(env, parameters, result))
      case _ =>
    }

    val matchbox =  Matchbox(matchboxRows)

    // Now we need to apply the partial args, if they exist...
    if (partialArguments.nonEmpty) {
      val (appliedMatchbox, leftoverArguments) = applyMatchbox(matchbox, partialArguments)

      // Since the number of partial arguments is less than the arity of the rows,
      // then we will always receive a partially applied matchbox back... and we should
      // have no leftover arguments, either.
      assert(leftoverArguments.isEmpty)
      appliedMatchbox.asInstanceOf[Matchbox]
    } else {
      matchbox
    }
  }
}
