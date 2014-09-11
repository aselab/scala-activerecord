package com.github.aselab.activerecord.inner

import com.github.aselab.activerecord._
import reflections._
import validations.Validator
import java.lang.annotation.Annotation

trait ProductModel extends Product with Saveable {
  @dsl.Ignore
  lazy val _companion = ReflectionUtil.classToCompanion(getClass)
    .asInstanceOf[ProductModelCompanion[this.type]]
}

trait ProductModelCompanion[T <: ProductModel] {
  import ReflectionUtil._

  /** corresponding model class */
  protected lazy val targetClass = companionToClass(this).asInstanceOf[Class[T]]

  /**
   * Create a new model object.
   */
  def newInstance: T = classInfo.factory.apply

  /** ProductModel class information */
  lazy val classInfo: ClassInfo[T] = ClassInfo(targetClass)

  /** ProductModel fields information */
  lazy val fieldInfo: Map[String, FieldInfo] = classInfo.fieldInfo

  lazy val fields: List[FieldInfo] = fieldInfo.values.toList

  lazy val validatableFields: List[FieldInfo] =
    fields.filter(info => classOf[validations.Validatable].isAssignableFrom(info.fieldType))

  lazy val validators: Map[String, Seq[(Annotation, Validator[_])]] = {
    fieldInfo.map {
      case (name, info) => (name, info.annotations.flatMap { a =>
        Validator.get(a.annotationType).map(a -> _)
      })
    }.toMap
  }
}

