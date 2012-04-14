package com.github.aselab.activerecord

trait Annotations {
  import annotation.target._

  type Column = org.squeryl.annotations.Column

  type OptionType = org.squeryl.annotations.OptionType

  type Transient = org.squeryl.annotations.Transient

  /**
   * ignore field annotation.
   */
  type Ignore = annotations.Ignore @field

  /**
   * unique field annotation.
   */
  type Unique = annotations.Unique @field
}
