package com.github.aselab.activerecord

import org.squeryl.annotations.Transient
import java.lang.annotation.Annotation
import org.apache.commons.validator.GenericValidator.isEmail

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

trait Validator {
  def apply(value: Any): Seq[String]
}

abstract class ValidatorFactory[T <: Annotation](implicit m: Manifest[T]) {
  def apply(a: T): Validator
  def register = ValidatorFactory.register(this)
  def unregister = ValidatorFactory.unregister(this)
}

object ValidatorFactory {
  def apply[T <: Annotation](validate: (T, Any) => Seq[String])(implicit m: Manifest[T]) = new ValidatorFactory[T] {
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
    (_, value) => if (value != null && value.toString.isEmpty) Seq("this field is required") else Nil
  }

  val lengthValidatorFactory = ValidatorFactory[annotations.Length] { (a, value) =>
    val l = value.toString.length
    if (a.min <= l && l <= a.max) Nil else Seq("length error")
  }

  val rangeValidatorFactory = ValidatorFactory[annotations.Range] { (a, value) =>
    def range[T <% Ordered[T]](min: T, v: T, max: T) = Seq(
      (v < min, "must be greater than or equal to " + min),
      (v > max, "must be less than or equal to " + max)
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
      case b: Boolean => if (b) Nil else Seq("Must be checked")
      case _ => throw new Exception("Unsupported")
    }
  }

  val emailValidatorFactory = ValidatorFactory[annotations.Email] { (_, value) =>
    if (isEmail(value.toString)) Nil else Seq("Must be email format")
  }
}

trait ValidationSupport extends Validatable {self: ActiveRecordBase[_] =>
  import ReflectionUtil._

  abstract override def doValidate(): Unit = {
    self._companion.fieldInfo.foreach {
      case (name, _) =>
        val validators = _companion.validators(name)
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
