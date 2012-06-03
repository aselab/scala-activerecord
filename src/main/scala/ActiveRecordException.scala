package com.github.aselab.activerecord

class ActiveRecordException(msg: String) extends RuntimeException(msg)

object ActiveRecordException {
  def unsupportedType(name: String) =
    throw new ActiveRecordException("Unsupported type: " + name)

  def defaultConstructorRequired =
    throw new ActiveRecordException("Must implement default constructor")

  def cannotCreateInstance(className: String, cause: String) =
    throw new ActiveRecordException("Cannot create instance of " + className +
      "\ncause: " + cause)

  def optionValueMustBeSome =
    throw new ActiveRecordException("Cannot detect generic type parameter when a field's default value is None because of type erasure.")

  def traversableValueMustNotBeNil =
    throw new ActiveRecordException("Cannot detect generic type parameter when a field's default value is Nil because of type erasure.")

  def cannotDetectType(value: Any) =
    throw new ActiveRecordException("Cannot detect type of %s.".format(value))

  def unsupportedDriver(driver: String) =
    throw new ActiveRecordException("Unsupported database driver: " + driver)

  def missingDriver(driver: String) =
    throw new ActiveRecordException("Cannot load database driver: " + driver)

  def missingRelation =
    throw new ActiveRecordException("Cannot find definition of relation")

  def missingForeignKey(name: String) =
    throw new ActiveRecordException("Cannot find declaration of foreign key: " + name)
}

