package com.github.aselab.activerecord

import org.squeryl.annotations.Transient

/**
 * CRUDとコールバックのインタフェース
 */
trait CRUDable {
  /** 新規インスタンスフラグ */
  @Transient
  protected var _isNewInstance = true

  /** 新規インスタンスフラグ */
  def isNewInstance = _isNewInstance

  /**
   * 保存メソッド．
   *
   * 新規インスタンスの時はdoCreateを，
   * そうでなければdoUpdateを呼び出す．
   * 保存前と保存後にコールバックメソッドを呼び出す．
   */
  def save(): Boolean = {
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
   * 削除メソッド.
   * 削除前と削除後にコールバックメソッドを呼び出す．
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
   * 新規保存処理を行うメソッド.
   * 保存に成功した場合trueを，失敗した場合falseを返すように実装する．
   */
  protected def doCreate(): Boolean

  /**
   * 更新保存処理を行うメソッド.
   * 更新に成功した場合trueを，失敗した場合falseを返すように実装する．
   */
  protected def doUpdate(): Boolean

  /**
   * 削除処理を行うメソッド.
   * 削除に成功した場合trueを，失敗した場合falseを返すように実装する．
   */
  protected def doDelete(): Boolean

  /**
   * 保存前のコールバックメソッド.
   * デフォルトでは何もしないため，必要があれば派生クラスで
   * オーバーライドして実装する．
   *
   * saveメソッドが呼び出された時，実際の保存メソッドである
   * doCreate, doUpdateを実行する前に呼びだされる．
   */
  protected def beforeSave() {}

  /**
   * 保存後のコールバックメソッド.
   * デフォルトでは何もしないため，必要があれば派生クラスで
   * オーバーライドして実装する．
   *
   * saveメソッドが呼び出された時，実際の保存メソッドである
   * doCreate, doUpdateを実行した後に呼びだされる．
   * 保存に失敗した場合は呼び出されない．
   */
  protected def afterSave() {}

  /**
   * 新規作成前のコールバックメソッド.
   * デフォルトでは何もしないため，必要があれば派生クラスで
   * オーバーライドして実装する．
   *
   * saveメソッドが呼び出された時，実際の保存メソッドである
   * doCreateを実行する前に呼びだされる．
   */
  protected def beforeCreate() {}

  /**
   * 新規作成後のコールバックメソッド.
   * デフォルトでは何もしないため，必要があれば派生クラスで
   * オーバーライドして実装する．
   *
   * saveメソッドが呼び出された時，実際の保存メソッドである
   * doCreateを実行した後に呼びだされる．
   * 保存に失敗した場合は呼び出されない．
   */
  protected def afterCreate() {}

  /**
   * 更新前のコールバックメソッド.
   * デフォルトでは何もしないため，必要があれば派生クラスで
   * オーバーライドして実装する．
   *
   * saveメソッドが呼び出された時，実際の保存メソッドである
   * doUpdateを実行する前に呼びだされる．
   */
  protected def beforeUpdate() {}

  /**
   * 更新後のコールバックメソッド.
   * デフォルトでは何もしないため，必要があれば派生クラスで
   * オーバーライドして実装する．
   *
   * saveメソッドが呼び出された時，実際の保存メソッドである
   * doUpdateを実行した後に呼びだされる．
   * 保存に失敗した場合は呼び出されない．
   */
  protected def afterUpdate() {}

  /**
   * 削除前のコールバックメソッド.
   * デフォルトでは何もしないため，必要があれば派生クラスで
   * オーバーライドして実装する．
   *
   * deleteメソッドが呼び出された時，実際の削除メソッドである
   * doDeleteを実行する前に呼びだされる．
   */
  protected def beforeDelete() {}

  /**
   * 削除後のコールバックメソッド.
   * デフォルトでは何もしないため，必要があれば派生クラスで
   * オーバーライドして実装する．
   *
   * deleteメソッドが呼び出された時，実際の削除メソッドである
   * doDeleteを実行した後に呼びだされる．
   * 削除に失敗した場合は呼び出されない．
   */
  protected def afterDelete() {}
}
