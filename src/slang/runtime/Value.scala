package slang.runtime

import java.lang.{String => JString}

import slang.lex._
import slang.parse._

sealed trait Value {
  def getType: JString

  /** toSlangString returns the string representation of a value.
   * (c.f. toString will return the <i>debug</i> representation of a value). */
  def toSlangString: JString

  /* Type coersion functions */

  def asNothing: Value = throw new IllegalStateException("Cannot get Nothing from " + getType)

  def asDouble: Double = throw new IllegalStateException("Cannot get Double from " + getType)

  def tryAsDouble(where: Token): Double = throw new RuntimeError(where, "Cannot get Double from " + getType)

  def isHashable = true
}

object Value {
  case object Nothing extends Value {
    def getInstance: Value.Nothing.type = this

    override def getType: JString = "Nothing"

    override def toSlangString: JString = toString()

    override def toString: JString = "nothing"
  }

  case class Number(value: Double) extends Value {
    override def getType: JString = "Number"

    override def toSlangString: JString = toString()

    override def asDouble: Double = value

    override def tryAsDouble(where: Token): Double = value

    override def toString = {
      var text = JString.valueOf(value)
      if (text endsWith ".0") {
        text = text.substring(0, text.length() - 2)
      }
      text
    }
  }

  case class String(value: JString) extends Value {
    override def getType = "String"

    override def toSlangString = value

    override def toString = "\"" + value + "\""
  }

  case class Atom(name: JString) extends Value {
    override def getType = "Atom"

    override def toSlangString = toString

    override def toString = if (name contains ' ') {
      ":`" + name + "`"
    } else {
      ":" + name
    }
  }

  case class List(values: scala.List[Value]) extends Value {
    override def getType: JString = "List"

    override def toSlangString: JString = toString()

    override def toString: JString = values.map(_.toString).mkString("[", ", ", "]")
  }

  case class Lazy(environment: Environment, expr: Expr) extends Value {
    override def toString: JString = {
      var repr = environment.collapsedString
      repr += " { ... }"
      repr
    }

    override def getType: JString = "Lazy"

    override def toSlangString: JString = toString()

    override def isHashable: Boolean = false
  }

  trait Callable extends Value
  trait Box extends Callable

  case class Matchbox(rows: scala.List[Matchbox.Row]) extends Box {
    override def getType: JString = "Matchbox"

    override def toString: JString = {
      // TODO(michael): Chris pls help make this pretty.
      rows.map({
        _.toSlangString
      }).mkString("{ ", ", ", " }")
    }

    override def toSlangString: JString = toString()

    override def isHashable: Boolean = false
  }

  object Matchbox {
    case class Row(innerEnvironment: Environment, parameters: scala.List[Pattern], guard: Option[Expr], result: Expr) {
      def toSlangString: JString = {
        innerEnvironment.shortString + " " + (parameters.map({
          _.toSlangString
        }).mkString(" ")) + " -> ..."
      }

      def withNewEnvironment: Row = {
        // TODO(michael): It's probably better to treat Environment objects as... immutable.
        Value.Matchbox.Row(Environment.fresh(innerEnvironment), parameters, guard, result)
      }
    }

    def from(env: Environment, ast: scala.List[Expr.MatchRow]): Matchbox =
      Matchbox(ast map { m => Row(env, m.patterns, m.guard, m.expr) })

    def toMatchboxOrHashbox(env: Environment, ast: scala.List[Expr.MatchRow]): Value = ast.reverse match {
      case literalPatterns if isHashboxable(literalPatterns, None) =>
        // NOTE: `literalPatterns` not reversed here, so our final hashmap has first pattern precedence.
        Value.Hashbox.from(env, literalPatterns, None)
      case extraPattern :: literalPatterns if isHashboxable(literalPatterns, Some(extraPattern)) =>
        Value.Hashbox.from(env, literalPatterns, Some(extraPattern))
      case _ => Matchbox.from(env, ast)
    }

    /** Whether this set of literal patterns are hashbox-able, that is:
     *  1. If there's at least one hashable pattern,
     *  2. All of the hashable patterns are the same arity, and
     *  3. The hashable patterns have no guards, and
     *  4. The additional non-hashable pattern (if exists) is the same arity.
     */
    def isHashboxable(literalPatterns: scala.List[Expr.MatchRow], extraPattern: Option[Expr.MatchRow]): Boolean =
      literalPatterns.nonEmpty && literalPatterns.forall(_.isHashable) && literalPatterns.forall(_.guard.isEmpty) && sameArity(literalPatterns map {
        _.patterns
      }) && extraPattern.forall(_.patterns.length == literalPatterns.head.patterns.length)

    // TODO: Helper fn, this doesn't need to be here...
    def sameArity[T](patterns: scala.List[scala.List[T]]): Boolean = patterns match {
      case Nil => true
      case first :: rest =>
        var arity = first.length
        rest forall {
          _.length == arity
        }
    }
  }

  case class Hashbox(partialArguments: scala.List[Value],
                    innerEnvironment: Environment,
                    arity: Int,
                    rows: Map[scala.List[Value], Expr],
                    extraRow: Option[Hashbox.Row]) extends Box {

    override def getType: JString = "Hashbox"

    override def toString: JString = {
      var rowStrings: scala.List[JString] = rows.map({
        case (k, _) => k.map(_.toSlangString).mkString(" ") + " -> ..."
      }).toList

      // Concatenate the extra row...
      extraRow match {
        case Some(row) =>
          // TODO(michael): Is there... a better append?
          rowStrings ++= scala.List(row.toSlangString)
        case None =>
      }

      // TODO(michael): Chris pls help make this pretty.
      val box = rowStrings.mkString("{(hashed) ", ", ", " }")

      partialArguments match {
        case Nil => box
        case _ => partialArguments.mkString("(", ", ", ") @ ") + box
      }
    }

    override def toSlangString: JString = toString()

    override def isHashable: Boolean = false
  }

  object Hashbox {
    case class Row(parameters: scala.List[Pattern], result: Expr) {
      def toSlangString: JString = parameters.map({
        _.toSlangString
      }).mkString(" ") + " -> ..."
    }

    def from(env: Environment, literalPatterns: scala.List[Expr.MatchRow], extraRow: Option[Expr.MatchRow]): Value = {
      val arity = literalPatterns.head.patterns.length
      var hashRows: Map[scala.List[Value], Expr] = Map.empty

      for (Expr.MatchRow(patterns, _, expr) <- literalPatterns) {
        hashRows += (patterns.map(_.asHashable) -> expr)
      }

      Value.Hashbox(Nil, Environment.fresh(env), arity, hashRows, extraRow map { m => Row(m.patterns, m.expr) })
    }
  }

  case class NativeFunction(func: Function2[Environment, scala.List[Value], (Value, scala.List[Value])]) extends Callable {
    override def getType: JString = "Native"
    override def toSlangString: JString = "<Native>"
    override def toString: JString = "<Native>"
  }

  case class Chain(boxes: scala.List[Callable], deferredArgs: scala.List[Thunk] = Nil) extends Callable {
    override def getType: JString = "Chain"
    override def toSlangString: JString = {
      deferredArgs.map(_.unevaluatedValue.toSlangString).mkString("[", ", ", "]") + boxes.map(_.toSlangString).mkString("\n |\n v\n")
    }
    override def toString: JString = toSlangString

    def head: Value = {
      boxes match {
        case Nil => throw new RuntimeError(null, "Tried to get head of an empty chain!")
        case (chain: Value.Chain) :: _ => chain.head
        case head :: _ => head
      }
    }
  }
}