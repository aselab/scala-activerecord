package com.github.aselab.activerecord

import inner._
import com.github.nscala_time.time.Imports._

trait Timestamps extends CRUDable {
  var createdAt: DateTime = null
  var updatedAt: DateTime = null

  abstract override protected def doCreate() = {
    val now = DateTime.now()
    createdAt = now
    updatedAt = now
    super.doCreate()
  }

  abstract override protected def doUpdate() = {
    updatedAt = DateTime.now()
    super.doUpdate()
  }
}

trait Datestamps extends CRUDable {
  var createdOn: LocalDate = null
  var updatedOn: LocalDate = null

  abstract override protected def doCreate() = {
    val today = LocalDate.now()
    createdOn = today
    updatedOn = today
    super.doCreate()
  }

  abstract override protected def doUpdate() = {
    updatedOn = LocalDate.now()
    super.doUpdate()
  }
}

