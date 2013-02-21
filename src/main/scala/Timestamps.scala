package com.github.aselab.activerecord

import java.util.Date
import java.sql.Timestamp
import inner._

trait Timestamps extends CRUDable {
  var createdAt: Timestamp = null
  var updatedAt: Timestamp = null

  abstract override protected def doCreate() = {
    val now = new Timestamp(System.currentTimeMillis)
    createdAt = now
    updatedAt = now
    super.doCreate()
  }

  abstract override protected def doUpdate() = {
    updatedAt = new Timestamp(System.currentTimeMillis)
    super.doUpdate()
  }
}

trait Datestamps extends CRUDable {
  var createdOn: Date = null
  var updatedOn: Date = null

  abstract override protected def doCreate() = {
    val today = new Date
    createdOn = today
    updatedOn = today
    super.doCreate()
  }

  abstract override protected def doUpdate() = {
    updatedOn = new Date
    super.doUpdate()
  }
}

