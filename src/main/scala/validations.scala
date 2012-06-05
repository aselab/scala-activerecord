package com.github.aselab.activerecord

import org.squeryl.annotations.Transient
import java.lang.annotation.Annotation
import org.apache.commons.validator.GenericValidator.isEmail

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

trait Validator {
  def apply(value: Any): Seq[(String, Seq[Any])]
}

abstract class ValidatorFactory[T <: Annotation](implicit m: Manifest[T]) {
  def apply(a: T): Validator
  def register = ValidatorFactory.register(this)
  def unregister = ValidatorFactory.unregister(this)
}

object ValidatorFactory {
  type Message = (String, Seq[Any])
  def message(msg: String, args: Any*): Message = (msg, args)

  def apply[T <: Annotation](validate: (T, Any) => Seq[Message])(implicit m: Manifest[T]) = new ValidatorFactory[T] {
    def apply(a: T) = new Validator {
      def apply(value: Any) = validate(a, value)
    }
  }

  type A = Class[_ <: Annotation]

  lazy val factories = collection.mutable.Map[A, ValidatorFactory[_ <: Annotation]](
    classOf[annotations.Required] -> requiredValidatorFactory,
    classOf[annotations.Length] -> lengthValidatorFactory,
    classOf[annotations.Range] -> rangeValidatorFactory,
    classOf[annotations.Email] -> emailValidatorFactory,
    classOf[annotations.Checked] -> checkedValidatorFactory
  )

  def register[T <: Annotation](factory: ValidatorFactory[T])(implicit m: Manifest[T]) =
    factories += (m.erasure.asInstanceOf[Class[T]] -> factory)

  def unregister(annotation: A): Unit = factories -= annotation

  def unregister[T <: Annotation](factory: ValidatorFactory[T])(implicit m: Manifest[T]): Unit =
    unregister(m.erasure.asInstanceOf[Class[T]])

  def get(annotation: A): Option[ValidatorFactory[Annotation]] =
    factories.get(annotation).asInstanceOf[Option[ValidatorFactory[Annotation]]]

  def get(annotation: Annotation): Option[ValidatorFactory[Annotation]] =
    get(annotation.annotationType)

  val requiredValidatorFactory = ValidatorFactory[annotations.Required] {
    (_, value) => if (value != null && value.toString.isEmpty)
      Seq(message("required")) else Nil
  }

  val lengthValidatorFactory = ValidatorFactory[annotations.Length] { (a, value) =>
    val l = value.toString.length
    Seq(
      (l < a.min, message("minLength", a.min)),
      (l > a.max, message("maxLength", a.max))
    ).collect {
      case (invalid, message) if invalid => message
    }
  }

  val rangeValidatorFactory = ValidatorFactory[annotations.Range] { (a, value) =>
    def range[T <% Ordered[T]](min: T, v: T, max: T) = Seq(
      (v < min, message("minValue", min)),
      (v > max, message("maxValue", max))
    ).collect {
      case (invalid, message) if invalid => message
    }

    value match {
      case v: Int => range(a.min.toInt, v, a.max.toInt)
      case v: Long => range(a.min.toLong, v, a.max.toLong)
      case v: Float => range(a.min.toFloat, v, a.max.toFloat)
      case v: Double => range(a.min, v, a.max)
      case _ => Nil
    }
  }

  val checkedValidatorFactory = ValidatorFactory[annotations.Checked] { (_, value) =>
    value match {
      case b: Boolean if !b => Seq(message("checked"))
      case _ => Nil
    }
  }

  val emailValidatorFactory = ValidatorFactory[annotations.Email] { (_, value) =>
    if (isEmail(value.toString)) Nil else Seq(message("invalid"))
  }
}

trait ValidationSupport extends Validatable {self: ProductModel =>
  import ReflectionUtil._

  abstract override def doValidate(): Unit = {
    self._companion.fieldInfo.foreach {
      case (name, _) =>
        val validators = _companion.validators(name)
        if (!validators.isEmpty) {
          (self.getValue[Any](name) match {
            case v: Option[_] => v
            case v => Some(v)
          }).foreach(value =>
            validators.foreach(_(value).collect{ case (message, args) =>
              errors.add(name, message, args:_*)
            })
          )
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
