package com.github.aselab.activerecord

import play.api._
import Play.current

class ActiveRecordPlugin(app: Application) extends Plugin {
  implicit val classLoader = Play.application.classloader
  lazy val activeRecordTables = current.configuration.getConfig("schema")
    .map(_.keys).getOrElse(List("models.Tables")).map(ActiveRecordTables.find)

  override def onStart() {
    activeRecordTables.foreach(_.initialize)
  }

  override def onStop() {
    activeRecordTables.foreach(_.cleanup)
  }
}

