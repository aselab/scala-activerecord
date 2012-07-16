package com.github.aselab.activerecord

case class ActiveRecordException(msg: String) extends RuntimeException(msg)

object ActiveRecordException {
  def unsupportedType(name: String) = apply("Unsupported type: " + name)

  def cannotCreateInstance(className: String, cause: String) =
    apply("Cannot create instance of " + className + "\ncause: " + cause)

  def unsupportedDriver(driver: String) =
    apply("Unsupported database driver: " + driver)

  def missingDriver(driver: String) =
    apply("Cannot load database driver: " + driver)

  def missingRelation =
    apply("Cannot find definition of relation")

  def missingForeignKey(name: String) =
    apply("Cannot find declaration of foreign key: " + name)

  def notfoundConfirmField(name: String) =
    apply("Cannot find declaration of confirmation field: " + name)

  def cannotCleanSession =
    apply("Must start session by ActiveRecordTables#start()")

  def scalaSig(c: Class[_]) =
    apply("Failed to extract ScalaSig from class " + c.getName)
}

