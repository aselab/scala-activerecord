package com.github.aselab

package object activerecord {
  object dsl extends org.squeryl.PrimitiveTypeMode {
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
}
