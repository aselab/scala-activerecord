package com.github.aselab.activerecord

import org.squeryl.annotations.Transient

trait Saveable {
  def save(): Boolean = false
  def isNewInstance: Boolean
}

/**
 * Interface of CRUD and callbacks.
 */
trait CRUDable extends Saveable {
  @Transient
  protected var _isNewInstance = true

  def isNewInstance = _isNewInstance

  /**
   * Save model.
   *
   * If isNewInstance flag is true, it calls doCreate method.
   * If not, it calls doUpdate method.
   * before and after callbacks are available.
   */
  override def save(): Boolean = {
    val onCreate = isNewInstance

    if (onCreate) beforeCreate() else beforeUpdate()
    beforeSave()

    val result = if (isNewInstance) doCreate() else doUpdate()

    if (result) {
      if (onCreate) afterCreate() else afterUpdate()
      afterSave()
      _isNewInstance = false
    }
    result
  }

  /**
   * Delete model.
   *
   * before and after callbacks are available.
   */
  def delete(): Boolean = !isNewInstance && {
    beforeDelete()

    val result = doDelete()

    if (result) {
      afterDelete()
      _isNewInstance = true
    }
    result
  }

  /**
   * Model creation.
   * Implement creation logic and return result of success or failure
   */
  protected def doCreate(): Boolean

  /**
   * Model update.
   * Implement update logic and return result of success or failure
   */
  protected def doUpdate(): Boolean

  /**
   * Model deletion.
   * Implement deletion logic and return result of success or failure
   */
  protected def doDelete(): Boolean

  /**
   * Callback method of before create and update.
   * 
   * You can override this method and implement logic if necessary.
   * Nothing is done by default.
   */
  protected def beforeSave() {}

  /**
   * Callback method of after create and update.
   * 
   * You can override this method and implement logic if necessary.
   * Nothing is done by default.
   * This is not called if failed to save.
   */
  protected def afterSave() {}

  /**
   * Callback method of before create.
   * 
   * You can override this method and implement logic if necessary.
   * Nothing is done by default.
   */
  protected def beforeCreate() {}

  /**
   * Callback method of after create.
   * 
   * You can override this method and implement logic if necessary.
   * Nothing is done by default.
   * This is not called if failed to create.
   */
  protected def afterCreate() {}

  /**
   * Callback method of before update.
   * 
   * You can override this method and implement logic if necessary.
   * Nothing is done by default.
   */
  protected def beforeUpdate() {}

  /**
   * Callback method of after update.
   * 
   * You can override this method and implement logic if necessary.
   * Nothing is done by default.
   * This is not called if failed to update.
   */
  protected def afterUpdate() {}

  /**
   * Callback method of before delete.
   * 
   * You can override this method and implement logic if necessary.
   * Nothing is done by default.
   */
  protected def beforeDelete() {}

  /**
   * Callback method of after delete.
   * 
   * You can override this method and implement logic if necessary.
   * Nothing is done by default.
   * This is not called if failed to delete.
   */
  protected def afterDelete() {}
}
