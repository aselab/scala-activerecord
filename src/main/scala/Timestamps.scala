package com.github.aselab.activerecord

import java.util.Date
import java.sql.Timestamp

trait Timestamps extends CRUDable {
  var createdAt: Timestamp = null
  var updatedAt: Timestamp = null

  abstract override protected def beforeCreate() = {
    createdAt = new Timestamp(System.currentTimeMillis)
    super.beforeCreate()
  }

  abstract override protected def beforeSave() = {
    updatedAt = new Timestamp(System.currentTimeMillis)
    super.beforeSave()
  }
}

trait Datestamps extends CRUDable {
  var createdOn: Date = null
  var updatedOn: Date = null

  abstract override protected def beforeCreate() = {
    createdOn = new Date
    super.beforeCreate()
  }

  abstract override protected def beforeSave() = {
    updatedOn = new Date
    super.beforeSave()
  }
}

