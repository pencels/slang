package slang.runtime

trait Callable {
  def apply(interpreter: Interpreter, args: List[Value]): Value
}