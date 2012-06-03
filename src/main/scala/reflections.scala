package com.github.aselab.activerecord

import java.lang.annotation.Annotation
import java.lang.reflect.{Field, ParameterizedType}
import scala.reflect.Manifest
import scala.util.control.Exception._
import annotations._

case class ClassInfo[T <: AnyRef](clazz: Class[T]) {
  import ClassInfo._

  lazy val factory: () => T = {
    if (isSeq(clazz)) {
      nilFactory
    } else {
      getFactory(clazz)
    }
  }.asInstanceOf[() => T]
}

object ClassInfo {
  val booleanFactory = () => new java.lang.Boolean(false)
  val intFactory = () => new java.lang.Integer(0)
  val longFactory = () => new java.lang.Long(0)
  val floatFactory = () => new java.lang.Float(0)
  val doubleFactory = () => new java.lang.Double(0.0)
  val nilFactory = () => Nil.asInstanceOf[AnyRef]
  def isSeq(clazz: Class[_]) = clazz.isAssignableFrom(Nil.getClass)

  val factories = collection.mutable.Map[Class[_], () => AnyRef](
    (classOf[String], () => ""),
    (classOf[java.lang.Boolean], booleanFactory),
    (classOf[Boolean], booleanFactory),
    (classOf[java.lang.Integer], intFactory),
    (classOf[Int], intFactory),
    (classOf[java.lang.Long], longFactory),
    (classOf[Long], longFactory),
    (classOf[java.lang.Float], floatFactory),
    (classOf[Float], floatFactory),
    (classOf[java.lang.Double], doubleFactory),
    (classOf[Double], doubleFactory),
    (classOf[java.util.Date], () => new java.util.Date(0)),
    (classOf[java.sql.Timestamp], () => new java.sql.Timestamp(0)),
    (classOf[java.util.UUID], () => new java.util.UUID(0, 0)),
    (classOf[BigDecimal], () => BigDecimal(0)),
    (classOf[Option[_]], () => None),
    (classOf[Array[Byte]], () => Array.empty[Byte])
  )

  def getFactory(clazz: Class[_]) = factories.getOrElseUpdate(clazz, {
    clazz.getConstructors.map(
      c => (c, c.getParameterTypes.toSeq)
    ).sortBy(_._2.size).toStream.flatMap {
      case (const, params) => allCatch.opt {
        // test creation parameters
        val facts = params.map(c =>
          ClassInfo(c.asInstanceOf[Class[AnyRef]]).factory)
        facts.foreach(_.apply)

        () => try {
          const.newInstance(facts.map(_.apply):_*).asInstanceOf[AnyRef]
        } catch {
          case e => ActiveRecordException.cannotCreateInstance(
            clazz.getName, e.getMessage)
        }
      }
    }.headOption.getOrElse(ActiveRecordException.cannotCreateInstance(
      clazz.getName, "No usable constructor is found. It is recommended to implement default constructor.")
    )
  })
}

case class FieldInfo(
  name: String, fieldTypeOption: Option[Class[_]],
  isOption: Boolean, isSeq: Boolean,
  annotations: Seq[Annotation] = Nil
) {
  private lazy val annotationMap = annotations.map {
    a => (a.annotationType.getSimpleName, a)
  }.toMap

  lazy val required = annotationMap.isDefinedAt("Required")
  lazy val ignored = annotationMap.isDefinedAt("Ignore")
  lazy val unique = annotationMap.isDefinedAt("Unique")

  lazy val fieldType = fieldTypeOption.getOrElse {
    if (isOption) ActiveRecordException.optionValueMustBeSome(name)
    else if (isSeq) ActiveRecordException.traversableValueMustNotBeNil(name)
    else ActiveRecordException.cannotDetectType(name)
  }
}

object FieldInfo {
  def apply(name: String, value: Any, field: Option[Field]): FieldInfo = value match {
    case Some(v) => apply(name, v, field).copy(isOption = true)
    case None => FieldInfo(name, None, true, false)

    case l: Traversable[_] => l.toSeq match {
      case Seq(v, _*) => apply(name, v, field).copy(isSeq = true)
      case Nil => FieldInfo(name, None, false, true)
    }

    case v: Any => FieldInfo(name, Option(v.getClass), false, false)
    case v => FieldInfo(name, field.map(_.getType), false, false)
  }

  def apply(name: String, value: Any): FieldInfo = apply(name, value, None)

  def apply(field: Field, value: Any): FieldInfo =
    apply(field.getName, value, Some(field)).copy(annotations = field.getAnnotations.toSeq)
}

trait ReflectionUtil {
  /**
   * returns companion object from class name
   * @param className class name
   */
  def classToCompanion(className: String): Any = {
    val cc = Class.forName(className + "$")
    cc.getField("MODULE$").get(cc)
  }

  /**
   * returns companion object from class
   * @param c class
   */
  def classToCompanion(c: Class[_]): Any = classToCompanion(c.getName)

  /**
   * returns corresponding class from companion object
   * @param c companion object
   */
  def companionToClass(c: Any) = Class.forName(c.getClass.getName.dropRight(1))

  implicit def toReflectable(o: Any) = new {
    val c = o.getClass

    def getValue[T](name: String) = c.getMethod(name).invoke(o).asInstanceOf[T]

    def setValue(name: String, value: Any) = {
      val f = c.getDeclaredField(name)
      f.setAccessible(true)
      f.set(o, value)
    }

    def getFields[T](implicit m: Manifest[T]) = c.getDeclaredFields.filter {
      f => m.erasure.isAssignableFrom(f.getType)
    }
  }

  def getGenericType(field: Field) = getGenericTypes(field).head
  def getGenericTypes(field: Field) = field.getGenericType.asInstanceOf[ParameterizedType].getActualTypeArguments.toList.map(_.asInstanceOf[Class[_]])
}

object ReflectionUtil extends ReflectionUtil
