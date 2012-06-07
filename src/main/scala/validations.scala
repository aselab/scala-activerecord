package com.github.aselab.activerecord

import org.squeryl.annotations.Transient
import java.lang.annotation.Annotation
import org.apache.commons.validator.GenericValidator.isEmail
import scala.util.DynamicVariable

class Errors(model: Class[_]) extends Iterable[ValidationError] {
  private val errors = collection.mutable.MutableList[ValidationError]()

  def iterator = errors.iterator

  def add(message: String) {
    errors += ValidationError(model, "", message)
  }

  def add(fieldName: String, message: String, args: Any*) {
    errors += ValidationError(model, fieldName, message, args:_*)
  }

  def clear = errors.clear
}

trait Validatable extends Saveable {
  @Transient
  @dsl.Ignore
  val errors = new Errors(getClass)

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

case class ValidationError(model: Class[_], key: String, message: String, args: Any*)

abstract class Validator[T <: Annotation](implicit m: Manifest[T]) {
  val _annotation = new DynamicVariable[Annotation](null)
  val _fieldName = new DynamicVariable[String](null)
  val _model = new DynamicVariable[Validatable](null)
  def annotation = _annotation.value.asInstanceOf[T]
  def fieldName = _fieldName.value
  def model = _model.value

  def errors = model.errors
  def validate(value: Any): Unit

  def validateWith(v: Any, a: Annotation, model: Validatable, name: String) = {
    _annotation.withValue(a) {
      _model.withValue(model) {
        _fieldName.withValue(name) {
          validate(v)
        }
      }
    }
  }

  def register = {
    Validator.register(this)
    this
  }

  def unregister = {
    Validator.unregister(this)
    this
  }
}

object Validator {
  type A = Class[_ <: Annotation]

  lazy val validators = collection.mutable.Map[A, Validator[_ <: Annotation]](
    classOf[annotations.Required] -> requiredValidator,
    classOf[annotations.Length] -> lengthValidator,
    classOf[annotations.Range] -> rangeValidator,
    classOf[annotations.Email] -> emailValidator,
    classOf[annotations.Checked] -> checkedValidator,
    classOf[annotations.Format] -> formatValidator,
    classOf[annotations.Confirm] -> confirmValidator
  )

  def register[T <: Annotation](validator: Validator[T])(implicit m: Manifest[T]) =
    validators += (m.erasure.asInstanceOf[Class[T]] -> validator)

  def unregister(annotation: A): Unit = validators -= annotation

  def unregister[T <: Annotation](validator: Validator[T])(implicit m: Manifest[T]): Unit =
    unregister(m.erasure.asInstanceOf[Class[T]])

  def get(annotation: A): Option[Validator[Annotation]] =
    validators.get(annotation).asInstanceOf[Option[Validator[Annotation]]]

  def get(annotation: Annotation): Option[Validator[Annotation]] =
    get(annotation.annotationType)

  def isBlank(value: Any) = value == null || value.toString.isEmpty

  val requiredValidator = new Validator[annotations.Required] {
    def validate(value: Any) =
      if (isBlank(value)) errors.add(fieldName, "required")
  }

  val lengthValidator = new Validator[annotations.Length] {
    def validate(value: Any) = {
      val l = if (value == null) 0 else value.toString.length
      val min = annotation.min
      val max = annotation.max
      if (l < min) errors.add(fieldName, "minLength", min)
      if (l > max) errors.add(fieldName, "maxLength", max)
    }
  }

  val rangeValidator = new Validator[annotations.Range] {
    def min = annotation.min
    def max = annotation.max

    def range[T <% Ordered[T]](min: T, v: T, max: T) = {
      if (v < min) errors.add(fieldName, "minValue", min)
      if (v > max) errors.add(fieldName, "maxValue", max)
    }

    def validate(value: Any) = value match {
      case v: Int => range(min.toInt, v, max.toInt)
      case v: Long => range(min.toLong, v, max.toLong)
      case v: Float => range(min.toFloat, v, max.toFloat)
      case v: Double => range(min, v, max)
      case _ =>
    }
  }

  val checkedValidator = new Validator[annotations.Checked] {
    def validate(value: Any) =
      if (value != true) errors.add(fieldName, "checked")
  }

  val emailValidator = new Validator[annotations.Email] {
    def validate(value: Any) = if (!isBlank(value) && !isEmail(value.toString))
      errors.add(fieldName, "invalid")
  }

  val formatValidator = new Validator[annotations.Format] {
    def validate(value: Any) = {
      val pattern = annotation.value
      if (!isBlank(value) && !isBlank(pattern) && pattern.r.findFirstIn(value.toString).isEmpty) errors.add(fieldName, "format")
    }
  }

  val confirmValidator = new Validator[annotations.Confirm] {
    def validate(value: Any) = {
      import ReflectionUtil._
      val confirmFieldName = fieldName + "Confirmation"
      val confirmValue = try {
        model.getValue[Any](confirmFieldName)
      } catch {
        case e => ActiveRecordException.notfoundConfirmField(confirmFieldName)
      }
      if (!isBlank(value) && value != confirmValue)
        errors.add(fieldName, "confirmation")
    }
  }
}

trait ValidationSupport extends Validatable {self: ProductModel =>
  import ReflectionUtil._

  abstract override def doValidate(): Unit = {
    _companion.fieldInfo.foreach {
      case (name, _) =>
        val validators = _companion.validators(name)
        if (!validators.isEmpty) {
          (self.getValue[Any](name) match {
            case v: Option[_] => v
            case v => Some(v)
          }).foreach { value => validators.foreach {
            case (a, validator) => validator.validateWith(value, a, this, name)
          }}
        }
    }
    super.doValidate()
  }
}

