package com.github.aselab.activerecord

import scala.language.experimental.macros
import scala.reflect.macros._

object MacroUtils {
  private[activerecord] def rawParams(c: whitebox.Context)(params: Seq[c.universe.Tree]) = {
    import c.universe._
    params.map {
      case q"scala.Predef.ArrowAssoc[${_}]($key).->[${_}]($value)" => (key.toString, value)
      case q"scala.Tuple2.apply[${_}, ${_}]($key, $value)" => (key.toString, value)
    }
  }

  private[activerecord] def modelFields(c: whitebox.Context)(modelType: c.universe.Type) = {
    import c.universe._
    val fields = modelType.decls ++ modelType.baseClasses.flatMap(_.asClass.toType.decls)
    fields.collect {
      case m: TermSymbol if m.isVal || m.isVar => ("\"" + m.name.toString.trim + "\"", m.typeSignature)
    }.toMap
  }

  private[activerecord] def validateField(c: whitebox.Context)
    (key: c.universe.Tree, value: c.universe.Tree, modelType: c.universe.Type): Unit =
    _validateFields(c)(Seq((key.toString, value)), modelType)

  private[activerecord] def validateFields(c: whitebox.Context)
    (params: Seq[c.universe.Tree], modelType: c.universe.Type): Unit =
    _validateFields(c)(rawParams(c)(params), modelType)

  private[activerecord] def _validateFields(c: whitebox.Context)
    (params: Seq[(String, c.universe.Tree)], modelType: c.universe.Type): Unit = {
      import c.universe.typeOf
      val fields = modelFields(c)(modelType)
      params.foreach { case (k, v) =>
        val inputType = v.tpe
        fields.get(k) match {
          case Some(typeSignature) if inputType <:< typeSignature || (typeSignature <:< typeOf[Option[_]] && typeSignature.contains(inputType.typeSymbol)) =>
          case Some(typeSignature) =>
            c.abort(c.enclosingPosition, s"type mismatch for ${k};\n  found   : ${inputType}\n  required: ${typeSignature}")
          case _ => c.abort(c.enclosingPosition, s"value ${k} is not a member of ${modelType}")
        }
      }
    }

  private[activerecord] def existsFields(c: whitebox.Context)
    (keys: Seq[c.universe.Tree], modelType: c.universe.Type): Unit = {
      val fields = modelFields(c)(modelType)
      keys.foreach { k =>
        if (fields.get(k.toString).isEmpty) {
          c.abort(c.enclosingPosition, s"value ${k} is not a member of ${modelType}")
        }
      }
    }
}

