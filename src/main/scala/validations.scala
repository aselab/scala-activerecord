package com.github.aselab.activerecord

import org.squeryl.annotations.Transient
import java.lang.annotation.Annotation
import org.apache.commons.validator.GenericValidator.isEmail
import scala.util.DynamicVariable
import java.util.Locale

class Errors(model: Class[_]) extends Iterable[ValidationError] {
  protected val errorList =
    collection.mutable.MutableList.empty[ValidationError]

  private var changed = false
  private var _errors = Map.empty[String, Seq[ValidationError]]
  protected def errors = {
    if (changed) {
      _errors = errorList.groupBy(_.key)
      changed = false
    }
    _errors
  }

  def iterator = errorList.iterator

  def add(message: String): Unit = add("", message)

  def add(fieldName: String, message: String, args: Any*) {
    errorList += ValidationError(model, fieldName, message, args:_*)
    changed = true
  }

  def exists(fieldName: String): Boolean = errors.isDefinedAt(fieldName)

  def get(fieldName: String): Seq[ValidationError] =
    errors.getOrElse(fieldName, Nil)

  def apply(fieldName: String): Seq[ValidationError] = get(fieldName)

  def global: Seq[ValidationError] = get("")

  def clear {
    errorList.clear
    changed = true
  }

  def messages(implicit locale: Locale = Locale.getDefault): Seq[String] =
    iterator.toSeq.map(_.translate)
}

trait Validatable extends Saveable {
  @Transient
  @dsl.Ignore
  val errors = new Errors(getClass)

  def globalErrors: Seq[ValidationError] = errors.global
  def fieldErrors: Seq[ValidationError] = errors.filterNot(_.isGlobal).toSeq

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

case class ValidationError(
  model: Class[_], key: String, message: String, args: Any*
) {
  lazy val translator = Config.translator

  def isGlobal: Boolean = key == ""

  def translate(implicit locale: Locale = Locale.getDefault): String =
    translator.translate(this)

  override def toString = translate
}

abstract class Validator[T <: Validator.AnnotationType](implicit m: Manifest[T]) {
  val _annotation = new DynamicVariable[Annotation](null)
  val _fieldName = new DynamicVariable[String](null)
  val _model = new DynamicVariable[Validatable](null)
  def annotation = _annotation.value.asInstanceOf[T]
  def fieldName = _fieldName.value
  def model = _model.value

  def errors = model.errors
  def message(default: String) =
    Option(annotation.message).filter(!_.isEmpty).getOrElse(default)

  def validate(value: Any): Unit

  def validateWith(v: Any, a: Annotation, model: Validatable, name: String) = {
    _annotation.withValue(a) {
      _model.withValue(model) {
        _fieldName.withValue(name) {
          val skip = (annotation.on match {
            case "save" => false
            case "create" if model.isNewInstance => false
            case "update" if !model.isNewInstance => false
            case _ => true
          })
          if (!skip) validate(v)
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
  type AnnotationType = Annotation with ({
    def message(): String
    def on(): String
  })
  type A = Class[_ <: Annotation]

  lazy val validators = collection.mutable.Map[A, Validator[_ <: AnnotationType]](
    classOf[annotations.Required] -> requiredValidator,
    classOf[annotations.Length] -> lengthValidator,
    classOf[annotations.Range] -> rangeValidator,
    classOf[annotations.Email] -> emailValidator,
    classOf[annotations.Checked] -> checkedValidator,
    classOf[annotations.Format] -> formatValidator,
    classOf[annotations.Confirm] -> confirmValidator
  )

  def register[T <: AnnotationType](validator: Validator[T])(implicit m: Manifest[T]) =
    validators += (m.erasure.asInstanceOf[A] -> validator)

  def unregister(annotation: A): Unit = validators -= annotation

  def unregister[T <: AnnotationType](validator: Validator[T])(implicit m: Manifest[T]): Unit =
    unregister(m.erasure.asInstanceOf[Class[T]])

  def get(annotation: A): Option[Validator[AnnotationType]] =
    validators.get(annotation).asInstanceOf[Option[Validator[AnnotationType]]]

  def get(annotation: Annotation): Option[Validator[AnnotationType]] =
    get(annotation.annotationType)

  def isBlank(value: Any) = value == null || value.toString.isEmpty

  val requiredValidator = new Validator[annotations.Required] {
    def validate(value: Any) =
      if (isBlank(value)) errors.add(fieldName, message("required"))
  }

  val lengthValidator = new Validator[annotations.Length] {
    def validate(value: Any) {
      val l = if (value == null) 0 else value.toString.length
      if (l == 0) return

      val min = annotation.min
      val max = annotation.max
      if (annotation.message.isEmpty) {
        if (l < min) errors.add(fieldName, "minLength", min)
        if (l > max) errors.add(fieldName, "maxLength", max)
      } else {
        if (l < min || l > max) errors.add(fieldName, annotation.message)
      }
    }
  }

  val rangeValidator = new Validator[annotations.Range] {
    def min = annotation.min
    def max = annotation.max

    def range[T <% Ordered[T]](min: T, v: T, max: T) = {
      if (annotation.message.isEmpty) {
        if (v < min) errors.add(fieldName, "minValue", min)
        if (v > max) errors.add(fieldName, "maxValue", max)
      } else {
        if (v < min || v > max) errors.add(fieldName, annotation.message)
      }
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
      if (value != true) errors.add(fieldName, message("checked"))
  }

  val emailValidator = new Validator[annotations.Email] {
    def validate(value: Any) = if (!isBlank(value) && !isEmail(value.toString))
      errors.add(fieldName, message("invalid"))
  }

  val formatValidator = new Validator[annotations.Format] {
    def validate(value: Any) = {
      val pattern = annotation.value
      if (!isBlank(value) && !isBlank(pattern) && pattern.r.findFirstIn(value.toString).isEmpty) errors.add(fieldName, message("format"))
    }
  }

  val confirmValidator = new Validator[annotations.Confirm] {
    def validate(value: Any) = {
      import ReflectionUtil._
      val confirmFieldName = fieldName + "Confirmation"
      val confirmValue = try {
        model.getValue[Any](confirmFieldName)
      } catch {
        case e =>
          throw ActiveRecordException.notfoundConfirmField(confirmFieldName)
      }
      if (!isBlank(value) && value != confirmValue)
        errors.add(fieldName, message("confirmation"))
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

