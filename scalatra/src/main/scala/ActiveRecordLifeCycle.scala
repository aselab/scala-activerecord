package com.github.aselab.activerecord.scalatra

import org.scalatra.LifeCycle
import javax.servlet.ServletContext
import com.github.aselab.activerecord._

trait ActiveRecordLifeCycle extends LifeCycle {
  val schemas = Config.loadSchemas()

  override def init(context: ServletContext): Unit = {
    schemas.foreach(_.initialize)
    super.init(context)
  }

  override def destroy(context: ServletContext): Unit = {
    super.destroy(context)
    schemas.foreach(_.cleanup)
  }
}

