package com.github.aselab.activerecord

import scala.concurrent.Future
import javax.inject._

import play.api._
import play.api.inject._

import com.google.inject.AbstractModule

@Singleton
class ActiveRecordInitialize @Inject() (
  lifecycle: ApplicationLifecycle,
  environment: Environment,
  configuration: Configuration,
  config: PlayConfig.type
) {
  implicit val classLoader = environment.classLoader
  lazy val activeRecordTables = configuration.getOptional[Configuration]("schema")
    .map(_.keys).getOrElse(List("models.Tables")).map(ActiveRecordTables.find)

  def onStart(): Unit ={
    activeRecordTables.foreach(_.initialize)
  }

  def onStop(): Unit = {
    activeRecordTables.foreach(_.cleanup)
  }

  lifecycle.addStopHook { () =>
    Future.successful(onStop())
  }

  onStart()
}

class ActiveRecordPlayModule extends AbstractModule {
  override def configure(): Unit = {
    Seq(
      bind(PlayConfig.getClass.asInstanceOf[Class[PlayConfig.type]]).toInstance(PlayConfig),
      bind(classOf[ActiveRecordInitialize]).asEagerSingleton()
    )
  }
}
