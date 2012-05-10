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

trait Validator {
  def apply(value: Any): Seq[String]
}

trait ValidatorFactory[T <: Annotation] {
  def apply(a: T): Validator
}

object ValidatorFactory {
  def apply[T <: Annotation](validate: (T, Any) => Seq[String]) = new ValidatorFactory[T] {
    def apply(a: T) = new Validator {
      def apply(value: Any) = validate(a, value)
    }
  }

  type A = Class[_ <: Annotation]
  lazy val factories = collection.mutable.Map[A, ValidatorFactory[_ <: Annotation]](
    classOf[annotations.Required] -> requiredValidatorFactory,
    classOf[annotations.Length] -> lengthValidatorFactory
  )

  def register(annotation: A, factory: ValidatorFactory[_ <: Annotation]) = factories += (annotation -> factory)
  def unregister(annotation: A) = factories -= annotation

  def get(annotation: A): Option[ValidatorFactory[Annotation]] = factories.get(annotation).asInstanceOf[Option[ValidatorFactory[Annotation]]]
  def get(annotation: Annotation): Option[ValidatorFactory[Annotation]] = get(annotation.annotationType)

  val requiredValidatorFactory = ValidatorFactory[annotations.Required] {
    (_, value) => if (value != null && value.toString.isEmpty) Seq("this field is required") else Nil
  }

  val lengthValidatorFactory = ValidatorFactory[annotations.Length] { (a, value) =>
    val l = value.toString.length
    if (a.min <= l && l <= a.max) Nil else Seq("length error")
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
