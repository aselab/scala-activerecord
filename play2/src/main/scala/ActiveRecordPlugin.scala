package com.github.aselab.activerecord

import play.api._

class ActiveRecordPlugin(app: Application) extends Plugin {
  lazy val activeRecordTables = new PlayConfig().schema

  override def onStart() {
    activeRecordTables.initialize
  }

  override def onStop() {
    activeRecordTables.cleanup
  }
}

