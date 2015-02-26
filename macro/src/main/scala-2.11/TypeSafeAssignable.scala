package com.github.aselab.activerecord

import scala.language.experimental.macros
import scala.reflect.macros._

trait TypeSafeAssignable {
  private[this] def rawParams(c: whitebox.Context)(params: Seq[c.universe.Tree]) = {
    import c.universe._
    params.map {
      case q"scala.this.Predef.ArrowAssoc[${_}]($key).->[${_}]($value)" => (key.toString, value)
    }
  }

  private[this] def modelFields(c: whitebox.Context)(modelType: c.universe.Type) = {
    import c.universe._
    val fields = modelType.decls ++ modelType.baseClasses.flatMap(_.asClass.toType.decls)
    fields.collect {
      case m: TermSymbol if m.isVal || m.isVar => ("\"" + m.name.toString.trim + "\"", m.typeSignature)
    }.toMap
  }

  private[this] def validateFields(c: whitebox.Context)(params: Seq[c.universe.Tree], modelType: c.universe.Type) = {
    import c.universe._
    val fields = modelFields(c)(modelType)
    rawParams(c)(params).foreach { case (k, v) =>
      val inputType = v.tpe
      fields.get(k) match {
        case Some(typeSignature) if inputType <:< typeSignature || typeSignature.contains(inputType.typeSymbol) =>
        case Some(typeSignature) =>
          c.abort(c.enclosingPosition, s"type mismatch for ${k};\n  found   : ${inputType}\n  required: ${typeSignature}")
        case _ => c.abort(c.enclosingPosition, s"value ${k} is not a member of ${modelType}")
      }
    }
  }

  def newInstance[T: c.WeakTypeTag](c: whitebox.Context)(data: c.Expr[(String, Any)]*): c.Tree = {
    import c.universe._
    val params = data.map(_.tree)
    val modelType = c.weakTypeOf[T]
    val thisScope = c.prefix.tree
    validateFields(c)(params, modelType)
    q"$thisScope.newInstance(Map(..$params))"
  }

  def assign[T: c.WeakTypeTag](c: whitebox.Context)(data: c.Expr[(String, Any)]*): c.Tree = {
    import c.universe._
    val params = data.map(_.tree)
    val thisScope = c.prefix.tree
    validateFields(c)(params, thisScope.tpe)
    q"$thisScope.assign(Map(..$params))"
  }
}
