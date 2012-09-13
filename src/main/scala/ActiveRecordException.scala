package com.github.aselab.activerecord

case class ActiveRecordException(msg: String) extends RuntimeException(msg)

object ActiveRecordException {
  def notInitialized: ActiveRecordException = apply("Not initialized")

  def unsupportedType(name: String): ActiveRecordException =
    apply("Unsupported type: " + name)

  def cannotCreateInstance(className: String, cause: String): ActiveRecordException =
    apply("Cannot create instance of " + className + "\ncause: " + cause)

  def unsupportedDriver(driver: String): ActiveRecordException =
    apply("Unsupported database driver: " + driver)

  def missingDriver(driver: String): ActiveRecordException =
    apply("Cannot load database driver: " + driver)

  def missingRelation: ActiveRecordException =
    apply("Cannot find definition of relation")

  def missingForeignKey(name: String): ActiveRecordException =
    apply("Cannot find declaration of foreign key: " + name)

  def notfoundConfirmationField(name: String): ActiveRecordException =
    apply("Cannot find declaration of confirmation field: " + name)

  def cannotCleanSession: ActiveRecordException =
    apply("Must start session by ActiveRecordTables#start()")

  def scalaSig(c: Class[_]): ActiveRecordException =
    apply("Failed to extract ScalaSig from class " + c.getName)
}

