package com.github.aselab.activerecord.inner

import com.github.aselab.activerecord._
import dsl._
import aliases._
import reflections.ReflectionUtil._

trait Optimistic extends CRUDable { self: AR =>
  protected val occVersionNumber = 0
  private def occVersion = occVersionNumber

  /** update with lock */
  abstract override protected def doUpdate() = try {
    val result = super.doUpdate()
    if (result) {
      this.setValue("occVersionNumber", occVersion + 1)
    }
    result
  } catch {
    case e: StaleUpdateException => throw ActiveRecordException.staleUpdate(e)
  }

  /** destroy with lock */
  abstract override protected def doDelete() = self.recordInDatabase match {
    case Some(m) if m.occVersion != occVersion =>
      throw ActiveRecordException.staleDelete(self.getClass.getName)
    case _ => super.doDelete()
  }
}
