package com.github.aselab.activerecord

import scala.language.experimental.macros
import scala.reflect.macros._

trait LegacyMethods {
  def newInstance[T: c.WeakTypeTag](c: Context)(data: c.Expr[(String, Any)]*): c.Expr[T] = {
    import c.universe._
    val params = data.map(_.tree).toList
    val modelType = c.weakTypeOf[T]
    val thisScope = c.prefix.tree
    c.Expr[T](q"$thisScope.newInstance.unsafeAssign(Map(..$params))")
  }

  def assign[T: c.WeakTypeTag](c: Context)(data: c.Expr[(String, Any)]*): c.Expr[T] = {
    import c.universe._
    val params = data.map(_.tree).toList
    val thisScope = c.prefix.tree
    c.Expr[T](q"$thisScope.unsafeAssign(Map(..$params))")
  }
}
