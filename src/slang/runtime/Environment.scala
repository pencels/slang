package slang.runtime

import slang.parse.AstPrinter

import scala.collection._

class Environment(val parent: Option[Environment]) {
  private val environment = new mutable.HashMap[String, Value]

  def define(name: String, value: Value): Unit = environment.put(name, value)

  def get(name: String): Value = tryGet(name) match {
    case Some(value) => value
    case _ => throw new RuntimeException("Unbound name '" + name + "'.")
  }

  def tryGet(name: String): Option[Value] = (environment.get(name), parent) match {
    case (Some(v), _) => Some(v)
    case (None, None) => None
    case (None, Some(p)) => p.tryGet(name)
  }

  def `with`(name: String, value: Value): Environment = {
    environment.put(name, value)
    this
  }

  def set(name: String, value: Value): Unit = {
    if (environment contains name) {
      environment.put(name, value)
    } else {
      parent match {
        case Some(p) => p.set(name, value)
        case None => 
          throw new RuntimeException("Name '" + name + "' assigned to but has not been declared.")
      }
    }
  }

  def shortString: String = {
    val keyValPairs = environment.map({ case (name, value) =>
      val valueStr = value match {
        case _: Value.Matchbox => "<Matchbox>"
        case _: Value.Hashbox => "<Hashbox>"
        case _: Value.Lazy => "{ ... }"
        case _ => new AstPrinter().print(value)
      }
      s"$name=$valueStr"
    }).toList

    val maybeTruncatedPairs = if (environment.size > 5) ("..." :: keyValPairs.takeRight(4)) else keyValPairs
    
    maybeTruncatedPairs.mkString("[", ", ", "]")
  }

  def collapsedString: String = {
    "{ <env> }"
  }

  override def toString: String = shortString

  def isEmpty: Boolean = environment.isEmpty

  def update(newBindings: Environment): Unit = environment ++= newBindings.environment
}

object Environment {
  /**
    * Returns a fresh environment with an existing environment as its parent.
    *
    * @param env The parent env.
    */
  def fresh(env: Environment) = new Environment(Some(env))

  /**
    * Returns an empty environment with no parent.
    */
  def empty = new Environment(None)
}