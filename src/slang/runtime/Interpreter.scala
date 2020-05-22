package slang.runtime

import slang.lex.Token
import slang.lex.TokenType
import slang.parse.Expr
import slang.parse.Pattern
import slang.parse.Stmt

import scala.annotation.tailrec

object Interpreter {
  val TRUE_ATOM: Value = Atom("true")
  val FALSE_ATOM: Value = Atom("false")
}

// TODO: rewrite all this visitor stuff to be just a match expr.
class Interpreter {
  def interpret(env: Environment, statements: List[Stmt]): Value = {
    var ret: Value = SlangNothing

    for (stmt <- statements) {
      ret = execute(env, stmt)
    }

    ret
  }

  def execute(env: Environment, stmt: Stmt): Value = {
    stmt match {
      case Stmt.Expression(expr) => strictEval(env, expr)
      case Stmt.Let(pattern, init) => {
        val value = eval(env, init)

        // TODO(michael): If we get types, we can verify the pattern is irrefutable first.
        if (!assign(env, pattern, Thunk.from(value))) {
          throw new FailedMatchException(pattern, value)
        }

        SlangNothing
      }
      case Stmt.Print(expr) => println(strictEval(env, expr).toSlangString); SlangNothing
      case Stmt.Match(_, _) => ??? // Should never reach this ???
    }
  }

  /** Convenience for strictCoerce(eval(env, exp)) */
  def strictEval(env: Environment, expr: Expr): Value = strictCoerce(eval(env, expr))

  /** Evaluate a lazy explicitly */
  def strictCoerce(value: Value): Value = value match {
    case Lazy(env, statements) => interpret(env, statements)
    case x => x
  }

  /** Evaluate a thunk explicitly, or uses the cached value if it has been evaluated once. */
  def strictCoerceThunk(thunk: Thunk): Value = thunk match {
    case Thunk(Some(value), _) => value
    case Thunk(_, unevaluatedValue) => {
      val value = strictCoerce(unevaluatedValue)
      thunk.cachedValue = Some(value)
      value
    }
  }

  def eval(env: Environment, expr: Expr): Value = expr match {
    case Expr.Assign(left, right) =>
      val value = eval(env, right)
      // NOTE(michael): I know, wrapping this in a thunk is kinda ugly here, but it's for a good cause.
      if (!assign(env, left, Thunk.from(value))) throw new RuntimeError(null, "Match failed.")
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
    case Expr.Block(statements) => Lazy(env, statements)
    case Expr.Matchbox(matches) => MatchBoques.from(env, matches)
  }

  def evalBinExpr(env: Environment, expr: Expr.Binary): Value = {
    val left = strictEval(env, expr.left)
    val right = strictEval(env, expr.right)
    val op = expr.op

    op.opType match {
      case TokenType.AT => call(env, right, List(left))
      case TokenType.PLUS =>
        (left, right) match {
          // We love overloaded operators
          case (SlangString(value), _) => SlangString(value + right.toSlangString)
          case (SlangList(leftValues), SlangList(rightValues)) => SlangList(leftValues ++ rightValues)
          case _ =>
            Number(left.tryAsDouble(op) + right.tryAsDouble(op))
        }
      case TokenType.MINUS =>
        Number(left.tryAsDouble(op) - right.tryAsDouble(op))
      case TokenType.STAR =>
        Number(left.tryAsDouble(op) * right.tryAsDouble(op))
      case TokenType.SLASH =>
        Number(left.tryAsDouble(op) / right.tryAsDouble(op))
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

  def evalPrefixOperator(env: Environment, unary: Expr.Prefix): Value = ???

  @tailrec
  final def call(env: Environment, callee: Value, args: List[Value]): Value = {
    strictCoerce(callee) match {
      case matchbox: MatchBoques => {
        applyMatchbox(matchbox, args) match {
          case (value, List()) => value
          case (value, rest) => call(env, value, rest)
        }
      }
      case SlangList(values) =>
        val elem = values(args.head match {
          case Number(n) if Math.floor(n) == n => n.toInt
          case _ => throw new RuntimeError(null /* TODO: We should put a token here. */ , "List index must be an integer.")
        })

        // If we have more args, then try to call elem as if she were a callable, sis.
        // This also allows us to do 2-D, 3-D, etc array access like `(arr 0 1)`
        args.tail match {
          case List() => elem
          case restArgs => call(env, elem, restArgs)
        }
      case _ => throw new RuntimeError(null, "Left expr of call must be a Matchbox or List.")
    }
  }

  def assign(env: Environment, pattern: Pattern, arg: Thunk): Boolean = {
    pattern match {
      case Pattern.Id(id) =>
        env.define(id.lexeme, arg.getValueMaybeCached)
        true
      case _: Pattern.Ignore => true
      case Pattern.Strict(inner) => {
        strictCoerceThunk(arg) // evaluate the thunk!
        assign(env, inner, arg)
      }
      case Pattern.Literal(_, value) => value == strictCoerceThunk(arg)
      case Pattern.SlangList(patterns) =>
        strictCoerceThunk(arg) match {
          case SlangList(values) => assignList(env, patterns, values)
          case _ => false
        }
      case Pattern.Spread(_) => throw new RuntimeError(null, "Unexpected spread pattern, not at end of list!")
    }
  }

  def assignList(env: Environment, patterns: List[Pattern], values: List[Value]): Boolean = (patterns, values) match {
    case (Nil, Nil) =>
      // We're at the end of the list... (or matching empty lists)
      true
    case (List(Pattern.Spread(spread)), values) => env.define(spread.lexeme, SlangList(values))
      // Match the rest of the list with a spread
      true
    case (pattern :: patterns, value :: values) =>
      // Do a 1:1 match of a list pattern and a value pattern
      assign(env, pattern, Thunk.from(value)) && assignList(env, patterns, values)
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
  def applyMatchbox(matchbox: MatchBoques, values: List[Value]): (Value, List[Value]) = {
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

      // This is an artifact of having to build up a new list...
      remainingRows = newRows.reverse
    }

    (MatchBoques(remainingRows), remainingValues)
  }
}
