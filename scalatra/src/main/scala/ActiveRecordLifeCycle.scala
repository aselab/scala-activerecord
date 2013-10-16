package com.github.aselab.activerecord.scalatra

import org.scalatra.LifeCycle
import javax.servlet.ServletContext

trait ActiveRecordLifeCycle extends LifeCycle {
  lazy val activeRecordTables = new ScalatraConfig().schema

  override def init(context: ServletContext) {
    activeRecordTables.initialize
    super.init(context)
  }

  override def destroy(context: ServletContext) {
    super.destroy(context)
    activeRecordTables.cleanup
  }
}

