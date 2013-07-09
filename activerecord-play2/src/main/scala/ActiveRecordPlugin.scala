package com.github.aselab.activerecord

import play.api._
import Play.current

class ActiveRecordPlugin(app: Application) extends Plugin {
  lazy val activeRecordTables = Config.loadSchemas("db.activerecord.schemas", current.configuration.underlying, Play.application.classloader)

  override def onStart() {
    activeRecordTables.foreach(_.initialize)
  }

  override def onStop() {
    activeRecordTables.foreach(_.cleanup)
  }
}

