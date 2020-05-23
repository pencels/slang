package slang.runtime

import slang.lex.Token
import slang.parse.Pattern.Literal
import slang.parse.Stmt.Match
import slang.parse.{Expr, Pattern, Stmt}

trait Value {
  def getType: String

  /** toSlangString returns the string representation of a value.
   * (c.f. toString will return the <i>debug</i> representation of a value). */
  def toSlangString: String

  /* Type coersion functions */

  def asNothing: Value = throw new IllegalStateException("Cannot get Nothing from " + getType)

  def asDouble: Double = throw new IllegalStateException("Cannot get Double from " + getType)

  def tryAsDouble(where: Token): Double = throw new RuntimeError(where, "Cannot get Double from " + getType)

  def isHashable = true
}

case object SlangNothing extends Value {
  def getInstance: SlangNothing.type = this

  override def getType: String = "Nothing"

  override def toSlangString: String = toString()

  override def toString: String = "nothing"
}

case class Number(value: Double) extends Value {
  override def getType: String = "Double"

  override def toSlangString: String = toString()

  override def asDouble: Double = value

  override def tryAsDouble(where: Token): Double = value

  override def toString: String = {
    var text = String.valueOf(value)
    if (text endsWith ".0") {
      text = text.substring(0, text.length() - 2)
    }
    text
  }
}

case class SlangString(value: String) extends Value {
  override def getType: String = "String"

  override def toSlangString: String = value

  override def toString: String = "\"" + value + "\""
}

case class Atom(name: String) extends Value {
  override def getType: String = "Atom"

  override def toSlangString: String = name

  override def toString: String = ":" + name
}

case class SlangList(values: List[Value]) extends Value {
  override def getType: String = "List"

  override def toSlangString: String = toString()

  override def toString: String = values.map(_.toString).mkString("[", ", ", "]")
}

case class Lazy(environment: Environment, statements: List[Stmt]) extends Value {
  override def toString: String = {
    var repr = environment.collapsedString
    repr += " { ... }"
    repr
  }

  override def getType: String = "Lazy"

  override def toSlangString: String = toString()

  override def isHashable: Boolean = false
}

case class Matchbox(rows: List[MatchboxRow]) extends Value {
  override def getType: String = "Matchbox"

  override def toString: String = {
    // TODO(michael): Chris pls help make this pretty.
    rows.map({
      _.toSlangString
    }).mkString("{ ", ", ", " }")
  }

  override def toSlangString: String = toString()

  override def isHashable: Boolean = false
}

object Matchbox {
  def from(env: Environment, ast: List[Match]): Matchbox =
    Matchbox(ast map { m => MatchboxRow(new Environment(env), m.patterns, m.expr) })

  def toMatchboxOrHashbox(env: Environment, ast: List[Match]): Value = ast.reverse match {
    case literalPatterns if isHashboxable(literalPatterns) =>
      // NOTE: `literalPatterns` not reversed here, so our final hashmap has first pattern precedence.
      Hashbox.from(env, literalPatterns, None)
    case extraPattern :: literalPatterns if isHashboxable(literalPatterns) =>
      Hashbox.from(env, literalPatterns, Some(extraPattern))
    case _ => Matchbox.from(env, ast)
  }

  /** Whether this set of literal patterns are matchbox-able */
  def isHashboxable(literalPatterns: List[Match]): Boolean =
    literalPatterns.nonEmpty && literalPatterns.forall(_.isHashable) && sameArity(literalPatterns map {
      _.patterns
    })

  // TODO: Helper fn, this doesn't need to be here...
  def sameArity[T](patterns: List[List[T]]): Boolean = patterns match {
    case Nil => true
    case first :: rest =>
      var arity = first.length
      rest forall {
        _.length == arity
      }
  }
}

case class MatchboxRow(innerEnvironment: Environment, parameters: List[Pattern], result: Expr) {
  def toSlangString: String = {
    innerEnvironment.shortString + " " + (parameters.map({
      _.toSlangString
    }).mkString(" ")) + " -> ..."
  }

  def withNewEnvironment: MatchboxRow = {
    // TODO(michael): It's probably better to treat Environment objects as... immutable.
    MatchboxRow(new Environment(innerEnvironment), parameters, result)
  }
}

case class Hashbox(partialArguments: List[Value],
                   innerEnvironment: Environment,
                   arity: Int,
                   rows: Map[List[Value], Expr],
                   extraRow: Option[HashboxRow]) extends Value {

  override def getType: String = "Hashbox"

  override def toString: String = {
    var rowStrings: List[String] = rows.map({
      case (k, _) => k.map(_.toSlangString).mkString(" ") + " -> ..."
    }).toList

    // Concatenate the extra row...
    extraRow match {
      case Some(row) =>
        // TODO(michael): Is there... a better append?
        rowStrings ++= List(row.toSlangString)
      case None =>
    }

    // TODO(michael): Chris pls help make this pretty.
    val box = rowStrings.mkString("{(hashed) ", ", ", " }")

    partialArguments match {
      case Nil => box
      case _ => partialArguments.mkString("(", ", ", ") @ ") + box
    }
  }

  override def toSlangString: String = toString()

  override def isHashable: Boolean = false
}

object Hashbox {
  def from(env: Environment, literalPatterns: List[Match], extraRow: Option[Match]): Value = {
    val arity = literalPatterns.head.patterns.length
    var hashRows: Map[List[Value], Expr] = Map.empty

    for (Match(patterns, expr) <- literalPatterns) {
      hashRows += (patterns.map(_.asHashable) -> expr)
    }

    Hashbox(Nil, new Environment(env), arity, hashRows, extraRow map { m => HashboxRow(m.patterns, m.expr) })
  }
}

case class HashboxRow(parameters: List[Pattern], result: Expr) {
  def toSlangString: String = parameters.map({
    _.toSlangString
  }).mkString(" ") + " -> ..."
}
