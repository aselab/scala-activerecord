package com.github.aselab.activerecord.validations

import com.github.aselab.activerecord._
import inner._
import java.lang.annotation.Annotation
import org.apache.commons.validator.GenericValidator.isEmail
import scala.util.DynamicVariable
import java.util.Locale
import scala.reflect.ClassTag
import scala.language.reflectiveCalls
import scala.language.existentials

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

  def iterator: Iterator[ValidationError] = errorList.iterator

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

  def clear: Unit = {
    errorList.clear
    changed = true
  }

  def messages(implicit locale: Locale = Locale.getDefault): Seq[String] =
    iterator.toSeq.map(_.translation)
}

trait Validatable extends Saveable { self: ProductModel =>
  @dsl.Transient
  @dsl.Ignore
  val errors = new Errors(getClass)

  @dsl.Transient
  @dsl.Ignore
  private var _validated = false

  def globalErrors: Seq[ValidationError] = errors.global
  def fieldErrors: Seq[ValidationError] = errors.filterNot(_.isGlobal).toSeq

  abstract override def save(): Boolean = validate && super.save

  def saveWithoutValidation(): Boolean = super.save

  def validate(): Boolean = {
    if (!_validated) {
      beforeValidation()
      doValidate()
      _validated = true
    }
    errors.isEmpty
  }

  def isValid: Boolean = validate

  def hasErrors: Boolean = !isValid

  def hasError(name: String): Boolean = errors.get(name).nonEmpty

  def clearErrors(): Unit = {
    errors.clear
    _validated = false
  }

  protected def doValidate(): Unit = {}

  protected def beforeValidation(): Unit = {}
}

case class ValidationError(
  model: Class[_], key: String, error: String, args: Any*
) {
  lazy val translator = Config.translator

  val isGlobal: Boolean = key == ""

  def label(implicit locale: Locale): String = translator.field(model, key)

  def message(implicit locale: Locale): String =
    translator.apply(error, args:_*)

  def translation(implicit locale: Locale): String = {
    if (isGlobal) message else label + " " + message
  }

  override def toString: String = translation(Locale.getDefault)

  def copy(model: Class[_] = this.model, key: String = this.key, error: String = this.error) =
    ValidationError(model, key, error, this.args:_*)
}

abstract class Validator[T <: Validator.AnnotationType](implicit m: ClassTag[T]) {
  val _annotation = new DynamicVariable[Annotation](null)
  val _fieldName = new DynamicVariable[String](null)
  val _model = new DynamicVariable[Validatable](null)
  def annotation: T = _annotation.value.asInstanceOf[T]
  def fieldName: String = _fieldName.value
  def model: Validatable = _model.value

  def errors: Errors = model.errors
  def message(error: String): String = Option(annotation.message).filter(!_.isEmpty)
    .getOrElse(Validator.ERROR_PREFIX + error)

  def validate(value: Any): Unit

  def validateOption(value: Option[Any]): Unit = value.foreach(validate)

  def validateWith(v: Option[Any], a: Annotation, model: Validatable, name: String): Unit = {
    _annotation.withValue(a) {
      _model.withValue(model) {
        _fieldName.withValue(name) {
          val skip = (annotation.on match {
            case "save" => false
            case "create" if model.isNewRecord => false
            case "update" if !model.isNewRecord => false
            case _ => true
          })
          if (!skip) validateOption(v)
        }
      }
    }
  }

  def register: Validator[T] = {
    Validator.register(this)
    this
  }

  def unregister: Validator[T] = {
    Validator.unregister(this)
    this
  }
}

object Validator {
  val ERROR_PREFIX = "activerecord.errors."

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
    classOf[annotations.Accepted] -> acceptedValidator,
    classOf[annotations.Format] -> formatValidator,
    classOf[annotations.Confirmation] -> confirmationValidator,
    classOf[annotations.StringEnum] -> stringEnumValidator,
    classOf[annotations.NumberEnum] -> numberEnumValidator,
    classOf[annotations.Unique] -> uniqueValidator
  )

  def register[T <: AnnotationType](validator: Validator[T])(implicit m: ClassTag[T]): Unit =
    validators += (m.runtimeClass.asInstanceOf[A] -> validator)

  def unregister(annotation: A): Unit = validators -= annotation

  def unregister[T <: AnnotationType](validator: Validator[T])(implicit m: ClassTag[T]): Unit =
    unregister(m.runtimeClass.asInstanceOf[Class[T]])

  def get(annotation: A): Option[Validator[AnnotationType]] =
    validators.get(annotation).asInstanceOf[Option[Validator[AnnotationType]]]

  def get(annotation: Annotation): Option[Validator[AnnotationType]] =
    get(annotation.annotationType)

  def isBlank(value: Any): Boolean = value == null || value.toString.isEmpty

  def confirmationFieldName(fieldName: String, a: annotations.Confirmation): String =
    Option(a.value).filter(!_.isEmpty).getOrElse(fieldName + "Confirmation")

  val requiredValidator = new Validator[annotations.Required] {
    def validate(value: Any): Unit =
      if (isBlank(value)) errors.add(fieldName, message("required"))

    override def validateOption(value: Option[Any]): Unit =
      validate(value.orNull)
  }

  val lengthValidator = new Validator[annotations.Length] {
    def validate(value: Any): Unit = if (!isBlank(value)) {
      val l = value.toString.length
      val min = annotation.min
      val max = annotation.max
      if (annotation.message.isEmpty) {
        if (l < min) errors.add(fieldName, ERROR_PREFIX + "minLength", min)
        if (l > max) errors.add(fieldName, ERROR_PREFIX + "maxLength", max)
      } else {
        if (l < min || l > max) errors.add(fieldName, annotation.message)
      }
    }
  }

  val rangeValidator = new Validator[annotations.Range] {
    private def min = annotation.min
    private def max = annotation.max

    def range[T <% Ordered[T]](min: T, v: T, max: T): Unit = {
      if (annotation.message.isEmpty) {
        if (v < min) errors.add(fieldName, ERROR_PREFIX + "minValue", min)
        if (v > max) errors.add(fieldName, ERROR_PREFIX + "maxValue", max)
      } else {
        if (v < min || v > max) errors.add(fieldName, annotation.message)
      }
    }

    def validate(value: Any): Unit = value match {
      case v: Int => range(min.toInt, v, max.toInt)
      case v: Long => range(min.toLong, v, max.toLong)
      case v: Float => range(min.toFloat, v, max.toFloat)
      case v: Double => range(min, v, max)
      case _ =>
    }
  }

  val acceptedValidator = new Validator[annotations.Accepted] {
    def validate(value: Any): Unit = value match {
      case v: Boolean => if (!v) errors.add(fieldName, message("accepted"))
      case _ =>
    }
  }

  val emailValidator = new Validator[annotations.Email] {
    def validate(value: Any): Unit =
      if (!isBlank(value) && !isEmail(value.toString)) {
        errors.add(fieldName, message("invalid"))
      }
  }

  val formatValidator = new Validator[annotations.Format] {
    def validate(value: Any): Unit = {
      val pattern = annotation.value
      if (!isBlank(value) && !isBlank(pattern) &&
        pattern.r.findFirstIn(value.toString).isEmpty) {
          errors.add(fieldName, message("format"))
      }
    }
  }

  val confirmationValidator = new Validator[annotations.Confirmation] {
    import reflections.ReflectionUtil._

    def validate(value: Any) :Unit = if (!isBlank(value)) {
      val name = confirmationFieldName(fieldName, annotation)
      val confirmationValue = try {
        model.getValue[Any](name)
      } catch {
        case e: NoSuchFieldException =>
          throw ActiveRecordException.notFoundConfirmationField(name)
      }
      if (value != confirmationValue) {
        errors.add(name, message("confirmation"), fieldName)
      }
    }
  }

  val stringEnumValidator = new Validator[annotations.StringEnum] {
    def validate(value: Any): Unit = {
      val values = annotation.value.toSeq.asInstanceOf[Seq[Any]]
      if (values.indexOf(value) < 0) {
        errors.add(fieldName, message("enum"), values.mkString(", "))
      }
    }
  }

  val numberEnumValidator = new Validator[annotations.NumberEnum] {
    def validate(value: Any): Unit = {
      val values = annotation.value.toSeq.asInstanceOf[Seq[Any]]
      if (values.indexOf(value) < 0) {
        errors.add(fieldName, message("enum"), values.mkString(", "))
      }
    }
  }

  val uniqueValidator = new Validator[annotations.Unique] {
    def validate(value: Any): Unit = model match {
      case m: ActiveRecordBase[_] if !m.recordCompanion.isUnique(fieldName, m)
        => errors.add(fieldName, message("unique"), value)
      case _ =>
    }
  }

}

trait ValidationSupport extends Validatable {self: ProductModel =>
  import reflections.ReflectionUtil._

  abstract override def doValidate(): Unit = {
    _companion.fieldInfo.foreach {
      case (name, info) if classOf[Validatable].isAssignableFrom(info.fieldType) =>
        info.toSeq[Validatable](this).map(_.validate)
      case (name, info) =>
        val validators = _companion.validators(name)
        if (!validators.isEmpty) {
          val value = self.getValue[Any](name) match {
            case v: Option[_] => v
            case v => Some(v)
          }
          validators.foreach  {
            case (a, validator) => validator.validateWith(value, a, this, name)
          }
        }
    }
    super.doValidate()
  }

  def saveEither: Either[Errors, this.type] = {
    if (save) Right[Errors, this.type](this) else Left(this.errors)
  }
}

