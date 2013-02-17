package com.github.aselab.activerecord.inner

import com.github.aselab.activerecord._

trait Annotations {
  import annotation.target._

  type Column = org.squeryl.annotations.Column

  type OptionType = org.squeryl.annotations.OptionType

  type Transient = org.squeryl.annotations.Transient @field

  /**
   * ignore field annotation.
   */
  type Ignore = annotations.Ignore @field

  /**
   * unique field annotation.
   */
  type Unique = annotations.Unique @field

  type Required = annotations.Required @field

  type Length = annotations.Length @field

  type Range = annotations.Range @field

  type Accepted = annotations.Accepted @field

  type Email = annotations.Email @field

  type Format = annotations.Format @field

  type Confirmation = annotations.Confirmation @field

  type StringEnum = annotations.StringEnum @field
  type NumberEnum = annotations.NumberEnum @field
}
