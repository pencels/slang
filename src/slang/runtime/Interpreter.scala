package slang.runtime

import slang.lex.TokenType
import slang.parse.{Expr, Pattern}

import scala.annotation.tailrec

case object Interpreter {
  val TRUE_ATOM: Value = Value.Atom("true")
  val FALSE_ATOM: Value = Value.Atom("false")

  def interpret(env: Environment, exprs: List[Expr]): Value = {
    var ret: Value = Value.Nothing

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
      case Value.Lazy(env, expr) =>
        val value = eval(Environment.fresh(env), expr)
        if (full) strictCoerce(value, full) else value
      case _ => value
    }
  }

  /** Evaluate a thunk explicitly, or uses the cached value if it has been evaluated once. */
  def strictCoerceThunk(thunk: Thunk, full: Boolean = false): Value = {
    val Thunk(maybeFullStrictValue, maybeStrictValue, unevaluatedValue) = thunk

    // Preemptively get the non-full strict value -- this is necessary for either full/non-full strict eval.
    val strictValue = maybeStrictValue getOrElse {
      val strictValue = strictCoerce(unevaluatedValue)
      thunk.strictValue = Some(strictValue)
      strictValue
    }

    // Eval full if needed, otherwise return the non-full strict value.
    if (full) {
      maybeFullStrictValue getOrElse {
        val fullStrictValue = strictCoerce(strictValue, full = true)
        thunk.fullStrictValue = Some(fullStrictValue)
        fullStrictValue
      }
    } else {
      strictValue
    }
  }

  def eval(env: Environment, expr: Expr): Value = expr match {
    case Expr.Assign(left, right) =>
      val value = eval(env, right)
      // NOTE(michael): I know, wrapping this in a thunk is kinda ugly here, but it's for a good cause.
      if (!assign(env, left, Thunk.from(value), define = false)) throw new RuntimeError(null, "Match failed.")
      Value.Nothing
    case bin: Expr.Binary => evalBinExpr(env, bin)
    case Expr.Call(callee, argExprs) =>
      val closure = eval(env, callee)
      val args = argExprs.map(eval(env, _))
      call(env, closure, args)
    case Expr.Grouping(inner) => eval(Environment.fresh(env), inner) // Groupings spawn a new env.
    case Expr.Id(name) => env.get(name)
    case Expr.Literal(value) => value
    case post: Expr.Postfix => evalPostfixExpr(env, post)
    case Expr.List(exprs) => Value.List(exprs.map(eval(env, _)))
    case unary: Expr.Prefix => evalPrefixOperator(env, unary)
    case Expr.Block(expr) => Value.Lazy(env, expr)
    case Expr.Matchbox(matches) => Value.Matchbox.toMatchboxOrHashbox(env, matches)
    case Expr.Let(pattern, init) => {
      val value = eval(env, init)

      // TODO(michael): If we get types, we can verify the pattern is irrefutable first.
      if (!assign(env, pattern, Thunk.from(value))) {
        throw new FailedMatchException(pattern, value)
      }

      Value.Nothing
    }
    case Expr.Print(expr) => println(eval(env, expr).toSlangString); Value.Nothing
    case Expr.MatchRow(_, _, _) => ??? // Should never reach this ???
    case Expr.Seq(exprs) => {
      if (exprs.isEmpty) {
        Value.Nothing
      } else {
        for (expr <- exprs.init) {
          strictEval(env, expr, full = true)
        }
        eval(env, exprs.last)
      }
    }
  }

  def mergeBoxes(left: Value, right: Value) = {
    (left, right) match {
      // We love overloaded operators
      case (Value.Matchbox(left), Value.Matchbox(right)) =>
        Value.Matchbox(left ++ right)
      case (Value.Matchbox(left), r: Value.Hashbox) =>
        val Value.Matchbox(right) = transmuteHashbox(r)
        Value.Matchbox(left ++ right)
      case (l: Value.Hashbox, r: Value.Hashbox) =>
        val Value.Matchbox(left) = transmuteHashbox(l)
        val Value.Matchbox(right) = transmuteHashbox(r)
        Value.Matchbox(left ++ right)
      case (l: Value.Hashbox, Value.Matchbox(right)) =>
        val Value.Matchbox(left) = transmuteHashbox(l)
        Value.Matchbox(left ++ right)
      case _ => ???
    }
  }

  def evalBinExpr(env: Environment, expr: Expr.Binary): Value = {
    val op = expr.op

    val left = eval(env, expr.left)
    val right = eval(env, expr.right)

    op.ty match {
      case TokenType.Operator("+") =>
        (left, right) match {
          // We love overloaded operators
          case (_: Value.Matchbox | _: Value.Hashbox, _: Value.Matchbox | _: Value.Hashbox) =>
            mergeBoxes(left, right)
          case _ =>
            // Use __dispatch__ to handle operator calls
            val dispatchTable = env.get("__dispatch__");
            val args = List(Value.Atom("+"), left, right)
            call(env, dispatchTable, args)
        }
      case TokenType.Operator(".") =>
        right match {
          case Value.List(values) => Value.List(left :: values)
          case _ =>
            throw new RuntimeError(expr.op, s"Operator `.` expects a List for its right operand")
        }
      case TokenType.Operator(op) =>
        // Use __dispatch__ to handle operator calls
        val dispatchTable = env.get("__dispatch__");
        val args = List(Value.Atom(op), left, right)
        call(env, dispatchTable, args)
      case _ => throw new RuntimeError(expr.op, s"Unexpected operator: ${expr.op.ty}")
    }
  }

  def evalPostfixExpr(env: Environment, expr: Expr.Postfix): Value = {
    val value = eval(env, expr.expr)
    expr.op.ty match {
      case TokenType.Operator(op) =>
        val value = eval(env, expr.expr)
        // Use __dispatch__ to handle operator calls
        val dispatchTable = env.get("__dispatch__");
        val args = List(Value.Atom("postfix" + op), value)
        call(env, dispatchTable, args)
      case _ => throw new RuntimeError(expr.op, s"Unexpected operator: ${expr.op.ty}")
    }
  }

  def evalPrefixOperator(env: Environment, expr: Expr.Prefix): Value = {
    val innerExpr = expr.expr
    expr.op.ty match {
      case TokenType.Operator("&") => Value.Lazy(env, innerExpr)
      case TokenType.Operator(op) =>
        val value = eval(env, expr.expr)
        // Use __dispatch__ to handle operator calls
        val dispatchTable = env.get("__dispatch__");
        val args = List(Value.Atom("prefix" + op), value)
        call(env, dispatchTable, args)
      case _ => ???
    }
  }

  @tailrec
  final def call(env: Environment, callee: Value, args: List[Value]): Value = {
    strictCoerce(callee, full = true) match {
      case matchbox: Value.Matchbox => {
        applyMatchbox(matchbox, args) match {
          case (value, Nil) => value
          case (value, rest) => call(env, value, rest)
        }
      }
      case hashbox: Value.Hashbox => {
        applyHashbox(hashbox, args) match {
          case (value, Nil) => value
          case (value, rest) => call(env, value, rest)
        }
      }
      case Value.NativeFunction(func) =>
        func(env, args) match {
          case (value, Nil) => value
          case (value, rest) => call(env, value, rest)
        }
      case callee => 
        // Use __dispatch__ to handle method calls on values that are not core callables.
        val dispatchTable = env.get("__dispatch__");
        val newArgs = args.head :: callee :: args.tail;
        call(env, dispatchTable, newArgs)
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
      case Pattern.Strict(inner, full) => assign(env, inner, Thunk.from(strictCoerceThunk(arg, full)), define)
      case Pattern.Literal(value) => value == strictCoerceThunk(arg, full = true)
      case Pattern.Cons(head, tail) =>
        strictCoerceThunk(arg, full = true) match {
          case Value.List(valHead :: valTail) =>
            assign(env, head, Thunk.from(valHead)) &&
              assign(env, tail, Thunk.from(Value.List(valTail)))
          case _ => false
        }
      case Pattern.List(patterns) =>
        strictCoerceThunk(arg, full = true) match {
          case Value.List(values) => assignList(env, patterns, values, define)
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
        env.define(spread.lexeme, Value.List(values))
      else
        env.set(spread.lexeme, Value.List(values))
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
  def applyMatchbox(matchbox: Value.Matchbox, values: List[Value]): (Value, List[Value]) = {
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
      var newRows: List[Value.Matchbox.Row] = Nil

      for (Value.Matchbox.Row(innerEnvironment, parameters, guard, result) <- remainingRows) {
        // I think we can assume every matchbox has at least one parameter.
        val parameter = parameters.head
        val remainingParameters = parameters.tail

        // Try to assign the pattern, mutating the environment if needed.
        if (assign(innerEnvironment, parameter, arg)) {
          // If the row is empty, check the guard.
          if (remainingParameters.isEmpty) {
            val passesGuard = guard map { eval(innerEnvironment, _) == Value.Atom("true") } getOrElse true
            if (passesGuard) {
              return (eval(innerEnvironment, result), remainingValues)
            }
          } else {
            // Otherwise, we'll just save this pattern for later!
            newRows = Value.Matchbox.Row(innerEnvironment, remainingParameters, guard, result) :: newRows
          }
        }
      }

      if (newRows.isEmpty) {
        throw new NoMatchException
      }

      // This is an artifact of having to build up a new list...
      remainingRows = newRows.reverse
    }

    (Value.Matchbox(remainingRows), remainingValues)
  }

  /** Apply a hashbox to a list of arguments.
   *
   * Given the arity N of the hashbox, and M parameters, this will return:
   * if N < M -> Applied hashbox, plus (M - N) leftover unapplied arguments,
   * if N = M -> Applied hashbox, and an empty list,
   * if N > M -> Deferred hashbox (saving the args for later), and an empty list.
   */
  def applyHashbox(hashbox: Value.Hashbox, args: List[Value]): (Value, List[Value]) = {
    val Value.Hashbox(partialArguments, innerEnvironment, arity, rows, extraRow) = hashbox

    (hashbox.partialArguments ++ args) splitAt hashbox.arity match {
      case (values, remainder) if values.length == hashbox.arity => {
        // We NEED to coerce these values to hash them correctly.
        val strictValues = values.map(strictCoerce(_, full = true))

        if (rows.contains(strictValues)) {
          // Cool, we just hash our value and call it a day.
          val expr = hashbox.rows(strictValues)
          (eval(Environment.fresh(hashbox.innerEnvironment), expr), remainder)
        } else extraRow match {
          // Otherwise, defer to the extra matchbox row...
          case Some(Value.Hashbox.Row(parameters, result)) =>
            val allValues = strictValues ++ remainder
            applyFinalHashboxRow(Environment.fresh(innerEnvironment), parameters, result, allValues)
          case None =>
            throw new NoMatchException
        }
      }
      case (partialArguments, Nil) if args.length < hashbox.arity => {
        // If we have less than the needed values, put her back together. Defer!
        // TODO(michael): Semantically, it might be useful to coerce these args of their lazy NOW instead of later?
        (Value.Hashbox(partialArguments, innerEnvironment, arity, rows, extraRow), Nil)
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
        throw new NoMatchException
      }
    }

    (Value.Matchbox(List(Value.Matchbox.Row(innerEnvironment, remainingParameters, None, result))), remainingValues)
  }

  def transmuteHashbox(hashbox: Value.Hashbox): Value.Matchbox = {
    val Value.Hashbox(partialArguments, env, _, rows, extraRow) = hashbox
    var matchboxRows = rows.map({case (k, v) => Value.Matchbox.Row(env, k map { Pattern.Literal(_) }, None, v)}).toList

    // Append the extra row, if exists.
    extraRow match {
      case Some(Value.Hashbox.Row(parameters, result)) =>
        matchboxRows ++= List(Value.Matchbox.Row(env, parameters, None, result))
      case _ =>
    }

    val matchbox =  Value.Matchbox(matchboxRows)

    // Now we need to apply the partial args, if they exist...
    if (partialArguments.nonEmpty) {
      val (appliedMatchbox, leftoverArguments) = applyMatchbox(matchbox, partialArguments)

      // Since the number of partial arguments is less than the arity of the rows,
      // then we will always receive a partially applied matchbox back... and we should
      // have no leftover arguments, either.
      assert(leftoverArguments.isEmpty)
      appliedMatchbox.asInstanceOf[Value.Matchbox]
    } else {
      matchbox
    }
  }
}
