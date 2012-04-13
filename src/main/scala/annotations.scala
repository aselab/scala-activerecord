package com.github.aselab.activerecord

import org.squeryl.annotations._

object Annotations {
  import annotation.target._

  /**
   * ignore field annotation.
   */
  type Ignore = annotations.Ignore @field

  /**
   * unique field annotation.
   */
  type Unique = annotations.Unique @field
}
