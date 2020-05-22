package slang.runtime

import slang.lex.Token
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

case class Lazy(statements: List[Stmt], environment: Environment) extends Value {
  override def toString: String = {
    var repr = environment.collapsedString
    repr += " { ... }"
    repr
  }

  override def getType: String = "Lazy"

  override def toSlangString: String = toString()
}

case class MatchBoques(rows: List[MatchboxRow]) extends Value {
  override def getType: String = "Matchbox"

  override def toString: String = {
    // TODO(michael): Chris pls help make this pretty.

    var repr = "{\n"

    for (row <- rows) {
      val envString = row.innerEnvironment.collapsedString
      val patternsString = row.parameters map {
        _.toString
      } reduceLeft {
        _ + " " + _
      }
      repr += envString + " " + patternsString + " -> ...\n"
    }

    repr + "}"
  }

  override def toSlangString: String = toString()
}

object MatchBoques {
  def from(env: Environment, ast: List[Match]) =
    MatchBoques(ast map { m => MatchboxRow(new Environment(env), m.patterns, m.expr) })
}

case class MatchboxRow(innerEnvironment: Environment, parameters: List[Pattern], result: Expr)
