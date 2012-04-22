package com.github.aselab.activerecord

import java.lang.annotation.Annotation
import java.lang.reflect.{Field, ParameterizedType}
import scala.reflect.Manifest
import annotations._

case class FieldInfo(
  name: String, fieldType: Class[_],
  isOption: Boolean, isSeq: Boolean,
  annotations: Seq[Annotation] = Nil
) {
  private lazy val annotationMap = annotations.map {
    a => (a.annotationType.getSimpleName, a)
  }.toMap

  lazy val ignored = annotationMap.isDefinedAt("Ignore")
  lazy val unique = annotationMap.isDefinedAt("Unique")
}

object FieldInfo {
  def apply(name: String, value: Any, field: Option[Field]): FieldInfo = value match {
    case Some(v) => apply(name, v, None).copy(isOption = true)
    case None => ActiveRecordException.optionValueMustBeSome

    case l: Traversable[_] => l.toSeq match {
      case Seq(v, _*) => apply(name, v, None).copy(isSeq = true)
      case Nil => ActiveRecordException.traversableValueMustNotBeNil
    }

    case v: Any => FieldInfo(name, v.getClass, false, false)
    case v =>
      val fieldType = field.map(_.getType).getOrElse(
        ActiveRecordException.cannotDetectType(v))
      FieldInfo(name, fieldType, false, false)
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
