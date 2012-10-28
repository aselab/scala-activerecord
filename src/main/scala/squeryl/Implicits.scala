package com.github.aselab.activerecord.squeryl

import com.github.aselab.activerecord._

object Implicits {
  import ReflectionUtil._

  implicit def anyToOption(o: Any) = new {
    def toOption[T]: Option[T] = o match {
      case null | None => None
      case Some(o) => Some(o.asInstanceOf[T])
      case o => Some(o.asInstanceOf[T])
    }
  }
}
