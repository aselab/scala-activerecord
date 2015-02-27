package com.github.aselab.activerecord

import scala.language.experimental.macros
import scala.reflect.macros._
import MacroUtils._

trait TypeSafeFinder {
  def findAllBy[T: c.WeakTypeTag](c: whitebox.Context)(conditions: c.Expr[(String, Any)]*): c.Tree = {
    import c.universe._
    val params = conditions.map(_.tree)
    val modelType = c.weakTypeOf[T]
    val thisScope = c.prefix.tree
    validateFields(c)(params, modelType)
    q"$thisScope.unsafeFindAllBy(..$params)"
  }

  def findAllByArg2[T: c.WeakTypeTag](c: whitebox.Context)(name: c.Tree, value: c.Tree): c.Tree = {
    import c.universe._
    val modelType = c.weakTypeOf[T]
    val thisScope = c.prefix.tree
    validateField(c)(name, value, modelType)
    q"$thisScope.unsafeFindAllBy($name, $value)"
  }

  def findBy[T: c.WeakTypeTag](c: whitebox.Context)(conditions: c.Expr[(String, Any)]*): c.Tree = {
    import c.universe._
    val params = conditions.map(_.tree)
    val modelType = c.weakTypeOf[T]
    val thisScope = c.prefix.tree
    validateFields(c)(params, modelType)
    q"$thisScope.unsafeFindBy(..$params)"
  }

  def findByArg2[T: c.WeakTypeTag](c: whitebox.Context)(name: c.Tree, value: c.Tree): c.Tree = {
    import c.universe._
    val modelType = c.weakTypeOf[T]
    val thisScope = c.prefix.tree
    validateField(c)(name, value, modelType)
    q"$thisScope.unsafeFindAllBy(($name, $value)).headOption"
  }

  def findByOrCreate[T: c.WeakTypeTag, S: c.WeakTypeTag](c: whitebox.Context)(m: c.Tree, fields: c.Tree*): c.Tree = {
    import c.universe._
    val modelType = c.weakTypeOf[T]
    val thisScope = c.prefix.tree
    existsFields(c)(fields, modelType)
    q"$thisScope.unsafeFindByOrCreate($m, ..$fields)"
  }

  def findById[T: c.WeakTypeTag](c: whitebox.Context)(id: c.Tree): c.Tree = {
    import c.universe._
    val modelType = c.weakTypeOf[T]
    val thisScope = c.prefix.tree
    val idString = "id"
    validateField(c)(q"$idString", id, modelType)
    q"""$thisScope.where(_.id === $id).headOption"""
  }
}
