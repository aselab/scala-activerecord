package com.github.aselab.activerecord

import java.lang.annotation.Annotation

trait ProductModel extends Product with Saveable {
  @dsl.Ignore
  lazy val _companion = ReflectionUtil.classToCompanion(getClass)
    .asInstanceOf[ProductModelCompanion[this.type]]
}

trait ProductModelCompanion[T <: ProductModel] {
  import ReflectionUtil._

  /** corresponding model class */
  protected val targetClass = companionToClass(this).asInstanceOf[Class[T]]

  /**
   * Create a new model object.
   */
  def newInstance: T = classInfo.factory.apply

  /** ProductModel class information */
  lazy val classInfo: ClassInfo[T] = ClassInfo(targetClass)

  /** ProductModel fields information */
  lazy val fieldInfo: Map[String, FieldInfo] = classInfo.fieldInfo

  lazy val formatFields: List[java.lang.reflect.Field] = classInfo.fields

  lazy val validators: Map[String, Seq[(Annotation, Validator[_])]] = {
    fieldInfo.map {
      case (name, info) => (name, info.annotations.flatMap { a =>
        Validator.get(a.annotationType).map(a -> _)
      })
    }.toMap
  }
}

