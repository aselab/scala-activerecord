package com.github.aselab.activerecord

import scala.language.experimental.macros
import scala.reflect.macros._
import MacroUtils._

trait TypeSafeAssignable {
  def newInstance[T: c.WeakTypeTag](c: whitebox.Context)(data: c.Expr[(String, Any)]*): c.Tree = {
    import c.universe._
    val params = data.map(_.tree)
    val modelType = c.weakTypeOf[T]
    val thisScope = c.prefix.tree
    validateFields(c)(params, modelType)
    q"$thisScope.newInstance.unsafeAssign(Map(..$params))"
  }

  def assign[T: c.WeakTypeTag](c: whitebox.Context)(data: c.Expr[(String, Any)]*): c.Tree = {
    import c.universe._
    val params = data.map(_.tree)
    val thisScope = c.prefix.tree
    validateFields(c)(params, thisScope.tpe)
    q"$thisScope.unsafeAssign(Map(..$params))"
  }
}
