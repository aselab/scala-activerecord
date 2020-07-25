package com.github.aselab.activerecord

import scala.reflect.macros._

trait Deprecations {
  def unsupportedInTransaction[A](c: whitebox.Context)(a: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    c.error(c.enclosingPosition, "dsl#inTransaction is deprecated. use ActiveRecordCompanion#inTransaction instead.")
    reify(a.splice)
  }
}
