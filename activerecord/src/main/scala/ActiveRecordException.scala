package com.github.aselab.activerecord

class ActiveRecordException(msg: String) extends RuntimeException(msg)

case class RecordInvalidException(msg: String) extends ActiveRecordException(msg)

case class RecordNotFoundException(msg: String) extends ActiveRecordException(msg)

case class StaleObjectException(msg: String) extends ActiveRecordException(msg)

case class SchemaSettingException(msg: String) extends ActiveRecordException(msg)

object ActiveRecordException {
  def apply(msg :String): ActiveRecordException = new ActiveRecordException(msg)

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

  def cannotLoadSchema(schemaClass: String): ActiveRecordException =
    apply("Cannot load schema class: " + schemaClass)

  def tableNotFound(name: String): SchemaSettingException =
    SchemaSettingException("Cannot find table definition of " + name)

  def recordNotFound: RecordNotFoundException =
    RecordNotFoundException("Cannot find record")

  def cannotRollback: ActiveRecordException =
    apply("Must call ActiveRecordTables#startTransaction before rollback")

  def scalaSig(c: Class[_]): ActiveRecordException =
    apply("Failed to extract ScalaSig from class " + c.getName)

  def saveFailed(errors: validations.Errors): RecordInvalidException =
    RecordInvalidException(errors.messages.mkString("\n"))

  def staleUpdate(e: dsl.StaleUpdateException): StaleObjectException =
    StaleObjectException(e.getMessage)

  def staleDelete(modelName: String): StaleObjectException =
    StaleObjectException("Attempted to delete a stale object: " + modelName)

  def recordMustBeSaved: ActiveRecordException =
    apply("owning side record must be saved")

  def notNullConstraint(name: String): ActiveRecordException =
    apply("foreign key '%s' has NotNull constraint".format(name))
}

