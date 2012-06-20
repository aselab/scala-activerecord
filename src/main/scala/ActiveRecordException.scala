package com.github.aselab.activerecord

case class ActiveRecordException(msg: String) extends RuntimeException(msg)

object ActiveRecordException {
  def unsupportedType(name: String) = apply("Unsupported type: " + name)

  def defaultConstructorRequired = apply("Must implement default constructor")

  def cannotCreateInstance(className: String, cause: String) =
    apply("Cannot create instance of " + className + "\ncause: " + cause)

  def optionValueMustBeSome(name: String) =
    apply("Cannot detect generic type of '%s' because of type erasure. Default value of Option field must not be None.".format(name))

  def traversableValueMustNotBeNil(name: String) =
    apply("Cannot detect generic type of '%s' because of type erasure. Default value of Seq field must not be Nil".format(name))

  def cannotDetectType(name: String) =
    apply("Cannot detect type of '%s'.".format(name))

  def unsupportedDriver(driver: String) =
    apply("Unsupported database driver: " + driver)

  def missingDriver(driver: String) =
    apply("Cannot load database driver: " + driver)

  def missingRelation =
    apply("Cannot find definition of relation")

  def missingForeignKey(name: String) =
    apply("Cannot find declaration of foreign key: " + name)

  def notfoundConfirmField(name: String) =
    apply("Cannot find confirmation field: " + name)

  def cannotCleanSession =
    apply("Required start session by ActiveRecordTables#start()")
}

