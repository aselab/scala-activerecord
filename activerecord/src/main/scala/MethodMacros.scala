package com.github.aselab.activerecord

import scala.quoted.*
import io.IO

/**
 * Unchecked operations that the type-safe finders expand to.
 * Mixed into [[ActiveRecord.Relation]]; separate so that the macros below can refer to it.
 */
trait UnsafeFinders[T, S] {
  def unsafeFind(id: Any): Option[S]
  def unsafeFindBy(condition: (String, Any), conditions: (String, Any)*): Option[S]
  def unsafeFindAllBy(name: String, value: Any): this.type
  def unsafeFindAllBy(condition: (String, Any), conditions: (String, Any)*): this.type
  def unsafeFindByOrCreate(m: T, field: String, fields: String*)(implicit ev: T =:= S): S
}

/**
 * Compile-time checks of field names and value types for `newInstance`, `assign` and the finders
 * (`find`, `findBy`, `findAllBy`, `findByOrCreate`). Each check expands to the corresponding
 * `unsafe*` call. Internal: called from inline methods only.
 */
object MethodMacros {

  // ---- shared checks --------------------------------------------------------------------------

  /** declared vals and vars of a model type and its base classes, with their types */
  private def modelFields(using q: Quotes)(model: q.reflect.TypeRepr): Map[String, q.reflect.TypeRepr] = {
    import q.reflect.*
    model.baseClasses.flatMap(_.declaredFields).distinctBy(_.name.trim)
      .map(f => f.name.trim -> model.memberType(f).widen).toMap
  }

  /** the pairs of a `("field" -> value, ...)` argument list written out at the call site */
  private def pairs(using Quotes)(args: Expr[Seq[(String, Any)]], method: String): Seq[Expr[(String, Any)]] =
    args match {
      case Varargs(es) => es
      case _ => quotes.reflect.report.errorAndAbort(
        s"$method needs its (\"field\" -> value) pairs written out; use the unsafe variant for a dynamic list", args)
    }

  private def keyValue(using Quotes)(pair: Expr[(String, Any)]): (Expr[String], Expr[Any]) = pair match {
    case '{ ($k: String) -> ($v: Any) } => (k, v)
    case '{ ($k: String, $v: Any) } => (k, v)
    case _ => quotes.reflect.report.errorAndAbort("expected a (\"field\" -> value) pair", pair)
  }

  private def fieldName(using Quotes)(key: Expr[String]): String = key.value.getOrElse(
    quotes.reflect.report.errorAndAbort("the field name must be a string literal; use the unsafe variant for a dynamic name", key))

  private def checkField(using q: Quotes)(model: q.reflect.TypeRepr, key: Expr[String], value: Expr[Any]): Unit = {
    import q.reflect.*
    val name = fieldName(key)
    modelFields(model).get(name) match {
      case None =>
        report.errorAndAbort(s"value $name is not a member of ${model.show}", key)
      case Some(fieldType) =>
        // the argument as written: inline parameters wrap it as `(arg: Any @InlineParam)`
        def written(t: Term): Term = t match {
          case Inlined(_, Nil, e) => written(e)
          case Typed(e, _) => written(e)
          case _ => t
        }
        val valueType = written(value.asTerm.underlyingArgument).tpe.widen
        val optionElement = fieldType <:< TypeRepr.of[Option[?]] && fieldType.typeArgs.headOption.exists(valueType <:< _)
        if (!(valueType <:< fieldType || optionElement))
          report.errorAndAbort(s"type mismatch for $name;\n  found   : ${valueType.show}\n  required: ${fieldType.show}", value)
    }
  }

  private def checkPairs(using q: Quotes)(model: q.reflect.TypeRepr, ps: Seq[Expr[(String, Any)]]): Unit =
    ps.foreach { p =>
      val (k, v) = keyValue(p)
      checkField(model, k, v)
    }

  private def nonEmpty[A](using Quotes)(es: Seq[Expr[A]], method: String, at: Expr[?]): Unit =
    if (es.isEmpty) quotes.reflect.report.errorAndAbort(s"$method needs at least one argument", at)

  // ---- newInstance / assign -------------------------------------------------------------------

  def newInstance[T <: ActiveModel: Type](self: Expr[ActiveModelCompanion[T]], data: Expr[Seq[(String, Any)]])(using Quotes): Expr[T] = {
    checkPairs(quotes.reflect.TypeRepr.of[T], pairs(data, "newInstance"))
    '{ $self.newInstance.unsafeAssign(Map($data*)) }
  }

  def assign[S <: IO: Type](self: Expr[S], data: Expr[Seq[(String, Any)]])(using Quotes): Expr[S] = {
    import quotes.reflect.*
    checkPairs(self.asTerm.tpe.widen, pairs(data, "assign"))
    '{ $self.unsafeAssign(Map($data*)).asInstanceOf[S] }
  }

  def assignMap[S <: IO: Type](self: Expr[S], data: Expr[Map[String, Any]])(using Quotes): Expr[S] = {
    import quotes.reflect.*
    val ps = data match {
      case '{ Map[String, Any](${ Varargs(es) }*) } => es
      case _ => report.errorAndAbort("assign needs a Map(...) literal; use unsafeAssign for a dynamic map", data)
    }
    checkPairs(self.asTerm.tpe.widen, ps)
    '{ $self.unsafeAssign($data).asInstanceOf[S] }
  }

  // ---- finders --------------------------------------------------------------------------------

  def find[T: Type, S: Type](self: Expr[UnsafeFinders[T, S]], id: Expr[Any])(using Quotes): Expr[Option[S]] = {
    checkField(quotes.reflect.TypeRepr.of[T], Expr("id"), id)
    '{ $self.unsafeFind($id) }
  }

  def findBy[T: Type, S: Type](self: Expr[UnsafeFinders[T, S]], conditions: Expr[Seq[(String, Any)]])(using Quotes): Expr[Option[S]] = {
    val ps = pairs(conditions, "findBy")
    nonEmpty(ps, "findBy", conditions)
    checkPairs(quotes.reflect.TypeRepr.of[T], ps)
    '{ $self.unsafeFindBy(${ ps.head }, ${ Varargs(ps.tail) }*) }
  }

  def findAllBy[T: Type, S: Type, R <: UnsafeFinders[T, S]: Type](self: Expr[R], conditions: Expr[Seq[(String, Any)]])(using Quotes): Expr[R] = {
    val ps = pairs(conditions, "findAllBy")
    nonEmpty(ps, "findAllBy", conditions)
    checkPairs(quotes.reflect.TypeRepr.of[T], ps)
    '{ $self.unsafeFindAllBy(${ ps.head }, ${ Varargs(ps.tail) }*).asInstanceOf[R] }
  }

  def findByNameValue[T: Type, S: Type](self: Expr[UnsafeFinders[T, S]], name: Expr[String], value: Expr[Any])(using Quotes): Expr[Option[S]] = {
    checkField(quotes.reflect.TypeRepr.of[T], name, value)
    '{ $self.unsafeFindBy(($name, $value)) }
  }

  def findAllByNameValue[T: Type, S: Type, R <: UnsafeFinders[T, S]: Type](self: Expr[R], name: Expr[String], value: Expr[Any])(using Quotes): Expr[R] = {
    checkField(quotes.reflect.TypeRepr.of[T], name, value)
    '{ $self.unsafeFindAllBy($name, $value).asInstanceOf[R] }
  }

  def findByOrCreate[T: Type, S: Type](self: Expr[UnsafeFinders[T, S]], m: Expr[T], fields: Expr[Seq[String]], ev: Expr[T =:= S])(using Quotes): Expr[S] = {
    import quotes.reflect.*
    val fs = fields match {
      case Varargs(es) => es
      case _ => report.errorAndAbort("findByOrCreate needs its field names written out; use unsafeFindByOrCreate for a dynamic list", fields)
    }
    nonEmpty(fs, "findByOrCreate", fields)
    val known = modelFields(TypeRepr.of[T])
    fs.foreach { f =>
      val name = fieldName(f)
      if (!known.contains(name)) report.errorAndAbort(s"value $name is not a member of ${TypeRepr.of[T].show}", f)
    }
    '{ $self.unsafeFindByOrCreate($m, ${ fs.head }, ${ Varargs(fs.tail) }*)(using $ev) }
  }
}
