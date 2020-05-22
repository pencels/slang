package slang.runtime

import slang.parse.{Expr, Pattern, Stmt}
import slang.util.TryObject._

case class Matchbox(matches: List[Stmt.Match], environment: Environment) extends Value with Callable {
  if (matches == null || environment == null) {
    throw new IllegalArgumentException("matches and environment must be non-null")
  }

  if (matches.exists(_.patterns.size != matches.head.patterns.size)) {
    throw new IllegalArgumentException("All pattern rows must be the same length.")
  }

  val arity: Int = matches.head.patterns.size

  def apply(interpreter: Interpreter, args: List[Value]): Value = {
    if (args.length < arity) {
      return DeferredMatchbox(matches, environment, args)
    }

    val slicedArgs = args.take(arity)

    val expr = resolve(slicedArgs).getOrElse {
      throw new NoMatchException()
    }
    val value = interpreter.eval(environment, expr)

    // Reapply rest of the arguments to result of application
    if (args.length > arity) {
      interpreter.call(value, args.drop(arity))
    } else {
      value
    }
  }

  private def resolve(args: List[Value]): Option[Expr] = resolve(matches, args)

  private def resolve(matches: List[Stmt.Match], args: List[Value]): Option[Expr] = {
    // Matchboxes with no rows always fail.
    if (matches.isEmpty)
      return None

    // If pattern length is zero, return the first row's expr.
    if (matches.head.patterns.isEmpty) {
      return Some(matches.head.expr)
    }

    val arg = args.head
    val (prefix, rest) = findPrefix(matches, arg)

    resolve(prefix, args.tail) orElse resolve(rest, args)
  }

  private def findPrefix(matches: List[Stmt.Match], arg: Value): (List[Stmt.Match], List[Stmt.Match]) = {
    val (prefix, rest) = matches.span(_.isVariableHead)
    if (prefix.nonEmpty) {
      val culled = prefix.map({ case Stmt.Match(patterns, expr) =>
        val newExpr = if (expr.isInstanceOf[Pattern.Ignore])
          expr
        else
          Expr.Block(List(Stmt.Let(patterns.head, Expr.Literal(arg)), Stmt.Expression(expr)))
        Stmt.Match(patterns.tail, newExpr)
      })
      return (culled, rest)
    }

    val unwrappedArg = arg match {
      case lazyVal: Lazy => lazyVal.getCachedValue(new Interpreter)
      case arg => arg
    }

    if (unwrappedArg.isElemental) {
      val (prefix, rest) = matches.span(_.isElementalHead) // Find elemental prefix
      val culled = prefix.filter(_.isLiteralValueHead(unwrappedArg)).map(_.tailMatch) // Filter out only equal values
      (culled, rest)
    } else if (unwrappedArg.isInstanceOf[SlangList]) {
      matches.span(_.isListHead)
      // TODO: do proper destructuring and whatnot,
    } else {
      throw new RuntimeException("We should never reach this point (:")
    }
  }

  private def choosePreMixtureRule(interpreter: Interpreter, args: List[Value]): Option[Matchbox] = {
    // Variable rule.
    val variableMatches = matches.filter(m => {
      val firstPattern = m.patterns.head
      firstPattern.isInstanceOf[Pattern.Id] || firstPattern.isInstanceOf[Pattern.Ignore]
    })

    if (variableMatches.length == matches.length) {
      val culledMatches = matches.map({ case Stmt.Match(patterns, expr) =>
        val newExpr = if (expr.isInstanceOf[Pattern.Ignore]) expr else Expr.Block(List(Stmt.Let(patterns.head, Expr.Literal(args.head))))
        Stmt.Match(patterns.tail, newExpr)
      })
      return Some(Matchbox(culledMatches, environment))
    }

    // Unwrap if arg is lazy. The following rules inspect the actual value of the arg.
    val unwrappedArg = args.head match {
      case lazyValue: Lazy => lazyValue.getCachedValue(interpreter)
      case arg => arg
    }

    // Elemental rule.
    // Match first elemental value pattern in the matchbox.
    if (unwrappedArg.isElemental) {
      val matchingRows = matches
        .filter(_.patterns.head.tryAsInstanceOf[Pattern.Literal].exists(_.value == unwrappedArg))
        .map(mat => Stmt.Match(mat.patterns.tail, mat.expr))

      if (matchingRows.nonEmpty) {
        return Some(Matchbox(matchingRows, environment))
      }
    }

    // List rule.
    if (unwrappedArg.isInstanceOf[SlangList]) {
      // TODO: find possible list matches, deconstruct into extra args for inner Mbox
    }

    None
  }

  override def getType: String = "Matchbox"

  override def asString: String = toString

  override def toString: String = s"Matchbox($matches)"

  override def isElemental: Boolean = false
}
