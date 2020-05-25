package slang.runtime

case class Thunk(var cachedValue: Option[Value], unevaluatedValue: Value) {
  // TODO(michael): Uh... a better name for this exists probably.
  def getValueMaybeCached: Value = cachedValue getOrElse unevaluatedValue
}

object Thunk {
  def from(value: Value) = Thunk(None, value)
  def fromStrict(value: Value) = Thunk(Some(value), value)
}
