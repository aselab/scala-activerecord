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

  def optionValueMustBeSome(name: String) =
    throw new ActiveRecordException("Cannot detect generic type of '%s' because of type erasure. Default value of Option field must not be None.".format(name))

  def traversableValueMustNotBeNil(name: String) =
    throw new ActiveRecordException("Cannot detect generic type of '%s' because of type erasure. Default value of Seq field must not be Nil".format(name))

  def cannotDetectType(name: String) =
    throw new ActiveRecordException("Cannot detect type of '%s'.".format(name))

  def unsupportedDriver(driver: String) =
    throw new ActiveRecordException("Unsupported database driver: " + driver)

  def missingDriver(driver: String) =
    throw new ActiveRecordException("Cannot load database driver: " + driver)

  def missingRelation =
    throw new ActiveRecordException("Cannot find definition of relation")

  def missingForeignKey(name: String) =
    throw new ActiveRecordException("Cannot find declaration of foreign key: " + name)

  def notfoundConfirmField(name: String) =
    throw new ActiveRecordException("Cannot find confirmation field: " + name)
}

