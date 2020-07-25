package com.github.aselab.activerecord.squeryl

import com.github.aselab.activerecord.reflections._
import scala.language.implicitConversions

object Implicits {
  implicit def fieldToExpression(field: FieldInfo) =
    new ExpressionConversion(field)
}
