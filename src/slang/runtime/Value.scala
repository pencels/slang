package slang.runtime

import scala.collection.mutable
import scala.ref.WeakReference

trait Value {
  def getType: String
  // toString will return the debug repr of a value. asString returns the string representation of a value.
  def asString: String

  def asNothing: Value = throw new IllegalStateException("Cannot get Nothing from " + getType)
  def asDouble: Double = throw new IllegalStateException("Cannot get Double from " + getType)

  def isElemental: Boolean
}

case object SlangNothing extends Value {
  def getInstance: SlangNothing.type = this
  override def getType: String = "Nothing"
  override def asString: String = toString()
  override def toString: String = "nothing"
  override def isElemental: Boolean = true
}

case class Number(value: Double) extends Value {
  override def getType: String = "Double"
  override def asString: String = toString()
  override def asDouble: Double = value
  override def toString: String = {
    var text = String.valueOf(value)
    if (text endsWith ".0") {
      text = text.substring(0, text.length() - 2)
    }
    text
  }
  override def isElemental: Boolean = true
}

case class SlangString(value: String) extends Value {
  override def getType: String = "String"
  override def asString: String = value
  override def toString: String = "\"" + value + "\""
  override def isElemental: Boolean = true
}

case class Atom private(name: String) extends Value {
  override def getType: String = "Atom"
  override def asString: String = name
  override def toString: String = ":" + name
  override def isElemental: Boolean = true
}

object Atom {
  private val cache = mutable.Map.empty[String, ref.WeakReference[Atom]]

  def apply(name: String): Atom = synchronized {
    cache.get(name) match {
      case Some(WeakReference(atom)) => atom
      case None =>
        val newAtom = new Atom(name)
        cache.put(name, WeakReference(newAtom))
        newAtom
    }
  }
}

case class SlangList(values: List[Value]) extends Value {
  override def getType: String = "List"
  override def asString: String = toString()
  override def toString: String = values.map(_.toString).mkString("[", ", ", "]")
  override def isElemental: Boolean = false
}
