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

  def assignMap[T: c.WeakTypeTag](c: Context)(data: c.Expr[Map[String, Any]]): c.Expr[T] = {
    import c.universe._
    val thisScope = c.prefix.tree
    c.Expr[T](q"$thisScope.unsafeAssign($data)")
  }

  def findAllBy[T: c.WeakTypeTag](c: Context)(conditions: c.Expr[(String, Any)]*): c.Expr[T] = {
    import c.universe._
    val params = conditions.map(_.tree)
    val thisScope = c.prefix.tree
    c.Expr[T](q"$thisScope.unsafeFindAllBy(..$params)")
  }

  def findAllByArg2[T: c.WeakTypeTag](c: Context)(name: c.Expr[String], value: c.Expr[Any]): c.Expr[T] = {
    import c.universe._
    val thisScope = c.prefix.tree
    c.Expr[T](q"$thisScope.unsafeFindAllBy($name, $value)")
  }

  def findBy[T: c.WeakTypeTag](c: Context)(conditions: c.Expr[(String, Any)]*): c.Expr[T] = {
    import c.universe._
    val params = conditions.map(_.tree)
    val thisScope = c.prefix.tree
    c.Expr[T](q"$thisScope.unsafeFindBy(..$params)")
  }

  def findByArg2[T: c.WeakTypeTag](c: Context)(name: c.Expr[String], value: c.Expr[Any]): c.Expr[T] = {
    import c.universe._
    val thisScope = c.prefix.tree
    c.Expr[T](q"$thisScope.unsafeFindAllBy(($name, $value)).headOption")
  }

  def findByOrCreate[T: c.WeakTypeTag, S: c.WeakTypeTag](c: Context)(m: c.Expr[T], fields: c.Expr[String]*): c.Expr[S] = {
    import c.universe._
    val thisScope = c.prefix.tree
    c.Expr[S](q"$thisScope.unsafeFindByOrCreate($m, ..$fields)")
  }

  def findById[T: c.WeakTypeTag](c: Context)(id: c.Expr[Any]): c.Expr[T] = {
    import c.universe._
    val modelType = c.weakTypeOf[T]
    val thisScope = c.prefix.tree
    c.Expr[T](q"""$thisScope.where(_.id === $id).headOption""")
  }
}
