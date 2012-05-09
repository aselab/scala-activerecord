package com.github.aselab.activerecord

import org.squeryl.annotations.Transient
import java.lang.annotation.Annotation

class Errors extends Iterable[ValidationError] {
  private val errors = collection.mutable.MutableList[ValidationError]()

  def iterator = errors.iterator

  def add(message: String) {
    errors += ValidationError("", message)
  }

  def add(fieldName: String, message: String) {
    errors += ValidationError(fieldName, message)
  }

  def clear = errors.clear
}

trait Validatable extends Saveable {
  @Transient
  val errors = new Errors

  def globalErrors = errors.filter(_.key == "")
  def fieldErrors = errors.filter(_.key != "")

  abstract override def save() = validate && super.save

  def saveWithoutValidation() = super.save

  def validate(): Boolean = {
    errors.clear
    beforeValidation()
    doValidate()
    errors.isEmpty
  }

  protected def doValidate(): Unit = {}

  protected def beforeValidation() {}
}

case class ValidationError(key: String, message: String)

object Validator {
  type Validator = (Any) => Seq[String]
  type A = Class[_ <: Annotation]
  private val validators = collection.mutable.Map[A, Validator](
    classOf[annotations.Required] -> requiredValidator
  )

  def register(annotation: A, validator: Validator) = validators += (annotation -> validator)
  def unregister(annotation: A) = validators -= annotation

  def get(annotation: A): Option[Validator] = validators.get(annotation)
  def get(annotation: Annotation): Option[Validator] = get(annotation.annotationType)

  val requiredValidator = {(value: Any) => Seq("this field is required")}
}

trait ValidationSupport extends Validatable {self: ActiveRecordBase[_] =>
  import ReflectionUtil._

  abstract override def doValidate(): Unit = {
    self._companion.fieldInfo.foreach {
      case (name, info) =>
        val validators = info.annotations.flatMap(Validator.get(_))
        if (!validators.isEmpty) {
          val value = self.getValue[Any](name)
          for (validator <- validators; message <- validator(value)) {
            errors.add(name, message)
          }
        }
    }
    super.doValidate()
  }
}

trait FormSupport[T <: ActiveRecord] {self: ActiveRecordCompanion[T] =>
  import ReflectionUtil._

  def bind(data: Map[String, String])(implicit source: T = self.newInstance): T = {
    source.assignFormValues(data)
    source
  }

  def unbind(m: T): Map[String, String] = throw new UnsupportedOperationException()
}
