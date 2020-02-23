package com.github.aselab.activerecord.inner

import com.github.aselab.activerecord._

trait Saveable {
  def save(): Boolean = false
  def isNewRecord: Boolean
}

/**
 * Interface of CRUD and callbacks.
 */
trait CRUDable extends Saveable {
  @dsl.Transient
  protected var _isNewRecord = true

  def isNewRecord: Boolean = _isNewRecord

  /**
   * Save model.
   *
   * If isNewRecord flag is true, it calls doCreate method.
   * If not, it calls doUpdate method.
   * before and after callbacks are available.
   */
  override def save(): Boolean = {
    val onCreate = isNewRecord

    beforeSave()
    if (onCreate) beforeCreate() else beforeUpdate()

    val result = if (onCreate) doCreate() else doUpdate()

    if (result) {
      if (onCreate) afterCreate() else afterUpdate()
      afterSave()
      _isNewRecord = false
    }
    result
  }

  /**
   * Delete model.
   *
   * before and after callbacks are available.
   */
  def delete(): Boolean = !isNewRecord && {
    beforeDelete()

    val result = doDelete()

    if (result) {
      afterDelete()
      _isNewRecord = true
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
  protected def beforeSave(): Unit = {}

  /**
   * Callback method of after create and update.
   *
   * You can override this method and implement logic if necessary.
   * Nothing is done by default.
   * This is not called if failed to save.
   */
  protected def afterSave(): Unit = {}

  /**
   * Callback method of before create.
   *
   * You can override this method and implement logic if necessary.
   * Nothing is done by default.
   */
  protected def beforeCreate(): Unit = {}

  /**
   * Callback method of after create.
   *
   * You can override this method and implement logic if necessary.
   * Nothing is done by default.
   * This is not called if failed to create.
   */
  protected def afterCreate(): Unit = {}

  /**
   * Callback method of before update.
   *
   * You can override this method and implement logic if necessary.
   * Nothing is done by default.
   */
  protected def beforeUpdate(): Unit = {}

  /**
   * Callback method of after update.
   *
   * You can override this method and implement logic if necessary.
   * Nothing is done by default.
   * This is not called if failed to update.
   */
  protected def afterUpdate(): Unit = {}

  /**
   * Callback method of before delete.
   *
   * You can override this method and implement logic if necessary.
   * Nothing is done by default.
   */
  protected def beforeDelete(): Unit = {}

  /**
   * Callback method of after delete.
   *
   * You can override this method and implement logic if necessary.
   * Nothing is done by default.
   * This is not called if failed to delete.
   */
  protected def afterDelete(): Unit = {}
}
