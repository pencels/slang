package slang.util

import scala.reflect.ClassTag

object TryObject {
  implicit class TryObject(val obj: Any) {
    def tryAsInstanceOf[T: ClassTag]: Option[T] = {
      obj match {
        case t: T => Some(t)
        case _ => None
      }
    }
  }
}