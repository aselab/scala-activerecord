package com.github.aselab.activerecord.squeryl

import com.github.aselab.activerecord._

object Implicits {
  implicit def fieldToExpression(field: FieldInfo) =
    new ExpressionConversion(field)
}
