package com.github.aselab.activerecord

case class ActiveRecordException(msg: String) extends RuntimeException(msg)

object ActiveRecordException {
  def notInitialized: ActiveRecordException = apply("Not initialized")

  def notImplemented: ActiveRecordException = apply("Not implemented")

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

  def notFoundConfirmationField(name: String): ActiveRecordException =
    apply("Cannot find declaration of confirmation field: " + name)

  def notFoundField(name: String): ActiveRecordException =
    apply("Cannot find declaration of field: " + name)

  def cannotLoadSchema(schemaClass: String) =
    apply("Cannot load schema class: " + schemaClass)

  def recordNotFound: ActiveRecordException =
    apply("Cannot find record")

  def cannotRollback: ActiveRecordException =
    apply("Must call ActiveRecordTables#startTransaction before rollback")

  def scalaSig(c: Class[_]): ActiveRecordException =
    apply("Failed to extract ScalaSig from class " + c.getName)

  def saveFailed(errors: validations.Errors): ActiveRecordException =
    apply(errors.messages.mkString("\n"))
}

