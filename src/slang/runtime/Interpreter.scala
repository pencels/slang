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
  val rootEnv = new Environment

  def evalTop(env: Environment, expr: Expr): Value = {
    var value = eval(env, expr)
    value match {
      case lazyValue: Lazy => value = lazyValue.getValue(this)
      case _ =>
    }
    value
  }

  def interpret(env: Environment, statements: List[Stmt]): Value = {
    var ret: Value = SlangNothing

    for (stmt <- statements) {
      ret = execute(env, stmt)
    }

    ret
  }

  def execute(env: Environment, stmt: Stmt): Value = {
    stmt match {
      case Stmt.Expression(expr) => evalTop(env, expr)
      case Stmt.Let(pattern, init) => destructureAssignment(pattern, eval(env, init), env); SlangNothing
      case Stmt.Print(expr) => println(evalTop(env, expr).asString); SlangNothing
      case Stmt.Match(_, _) => ??? // Should never reach this ???
    }
  }

  def eval(env: Environment, expr: Expr): Value = {
    expr match {
      case Expr.Assign(left, right) =>
        val value = eval(env, right)
        if (!destructureAssignment(left, value, env)) throw new RuntimeError(null, "Match failed.")
        value
      case bin: Expr.Binary => evalBinExpr(env, bin)
      case Expr.Call(callee, argExprs) =>
        val closure = eval(env, callee)
        val args = argExprs.map(eval(env, _))
        call(closure, args)
      case Expr.Grouping(inner) => eval(env, inner)
      case Expr.Id(name) => env.get(name)
      case Expr.Literal(value) => value
      case post: Expr.Postfix => evalPostfixExpr(env, post)
      case Expr.SlangList(exprs) => SlangList(exprs.map(eval(env, _)))
      case unary: Expr.Unary => evalUnaryExpr(env, unary)
      case Expr.Block(statements) => Lazy(statements, env)
      case Expr.Matchbox(matches) => Matchbox(matches, env)
    }
  }

  def evalBinExpr(env: Environment, expr: Expr.Binary): Value = {
    val left = evalTop(env, expr.left)
    val right = evalTop(env, expr.right)
    expr.op.`type` match {
      case TokenType.AT =>
        right match {
          case matchbox: Matchbox => matchbox.apply(this, List(left))
          case _ => throw new RuntimeError(null, "Expected matchbox.")
        }
      case TokenType.PLUS =>
        (left, right) match {
          case (SlangString(value), _) => SlangString(value + right.asString)
          case (SlangList(leftValues), SlangList(rightValues)) => SlangList(leftValues ++ rightValues)
          case _ =>
            checkNumberOperands(expr.op, left, right)
            Number(left.asDouble + right.asDouble)
        }
      case TokenType.MINUS =>
        checkNumberOperands(expr.op, left, right)
        Number(left.asDouble - right.asDouble)
      case TokenType.STAR =>
        checkNumberOperands(expr.op, left, right)
        Number(left.asDouble * right.asDouble)
      case TokenType.SLASH =>
        checkNumberOperands(expr.op, left, right)
        Number(left.asDouble / right.asDouble)
      case TokenType.EQEQ =>
        if (left == right) Interpreter.TRUE_ATOM
        else Interpreter.FALSE_ATOM
      case TokenType.NE =>
        if (left == right) Interpreter.FALSE_ATOM
        else Interpreter.TRUE_ATOM
      case TokenType.LT =>
        checkNumberOperands(expr.op, left, right)
        if (left.asDouble < right.asDouble) Interpreter.TRUE_ATOM
        else Interpreter.FALSE_ATOM
      case TokenType.LE =>
        checkNumberOperands(expr.op, left, right)
        if (left.asDouble <= right.asDouble) Interpreter.TRUE_ATOM
        else Interpreter.FALSE_ATOM
      case TokenType.GT =>
        checkNumberOperands(expr.op, left, right)
        if (left.asDouble > right.asDouble) Interpreter.TRUE_ATOM
        else Interpreter.FALSE_ATOM
      case TokenType.GE =>
        checkNumberOperands(expr.op, left, right)
        if (left.asDouble >= right.asDouble) Interpreter.TRUE_ATOM
        else Interpreter.FALSE_ATOM
      case _ => throw new RuntimeError(expr.op, s"Unexpected operator: ${expr.op.`type`}")
    }
  }

  def evalPostfixExpr(env: Environment, expr: Expr.Postfix): Value = {
    val value = eval(env, expr.expr)
    expr.op.`type` match {
      case TokenType.BANG =>
        checkNumberOperand(expr.op, value)
        var i = value.asDouble.intValue
        var n = 1

        while (i > 0) {
          n *= i
          i -= 1
        }
        Number(n)
      case _ => throw new RuntimeError(expr.op, s"Unexpected operator: ${expr.op.`type`}")
    }
  }

  def evalUnaryExpr(env: Environment, unary: Expr.Unary): Value = ???

  private def checkNumberOperand(op: Token, operand: Value): Unit = {
    operand match {
      case Number(_) => // Ok (:
      case _ => throw new RuntimeError(op, "Expected operand to be number.")
    }
  }

  private def checkNumberOperands(op: Token, left: Value, right: Value): Unit = {
    (left, right) match {
      case (Number(_), Number(_)) => // Ok (:
      case _ => throw new RuntimeError(op, "Expected operands to be numbers.")
    }
  }

  @tailrec
  final def call(callee: Value, args: List[Value]): Value = {
    val unwrappedCallee = callee match {
      case lazyCallee: Lazy => lazyCallee.getValue(this)
      case _ => callee
    }

    unwrappedCallee match {
      case matchbox: Matchbox => matchbox.apply(this, args)
      case SlangList(values) =>
        val indexArg = args.head
        val intIndex = indexArg match {
          case Number(n) if Math.floor(n) == n => n.toInt
          case _ => throw new RuntimeError(null, "List index must be an integer.")
        }
        val elem = values(intIndex)
        val restArgs = args.tail

        if (restArgs.isEmpty) elem
        else call(elem, restArgs)
      case _ => throw new RuntimeError(null, "Left expr of call must be a Matchbox.")
    }
  }

  def destructureAssignment(patterns: List[Pattern], args: List[Value], parent: Environment): Environment = { // Create a new env to hold new bindings.
    val env = new Environment(parent)
    val n = Math.min(patterns.size, args.size)
    for (i <- 0 until n) {
      if (!destructureAssignment(patterns(i), i, args, env)) return null
    }
    env
  }

  def destructureAssignment(pattern: Pattern, arg: Value, env: Environment): Boolean = {
    pattern match {
      case Pattern.Id(id) =>
        env.define(id.lexeme, arg)
        true
      case _: Pattern.Ignore => true
      case Pattern.Lazy(inner) => destructureAssignment(inner, arg, env)
      case Pattern.Literal(_, value) =>
        val unwrappedArg = arg match {
          case lazyArg: Lazy => lazyArg.getCachedValue(this)
          case _ => arg
        }
        value == unwrappedArg
      case Pattern.SlangList(patterns) =>
        val unwrappedArg = arg match {
          case lazyArg: Lazy => lazyArg.getCachedValue(this)
          case _ => arg
        }
        unwrappedArg match {
          case SlangList(values) =>
            // Check empty list pattern.
            if (patterns.isEmpty) values.isEmpty
            else {
              // TODO: Destructure list...
              false
            }
          case _ => false
        }
      case Pattern.Spread(_) => ???
    }
  }

  // Destructure pattern for arg i in arguments, globbing the rest of the arguments if the pattern is a spread pattern.
  private def destructureAssignment(pattern: Pattern, i: Int, args: List[Value], env: Environment): Boolean = {
    pattern match {
      case Pattern.Spread(id) =>
        env.define(id.lexeme, SlangList(args.slice(i, args.length)))
        true
      case _ =>
        destructureAssignment(pattern, args(i), env)
    }
  }
}
