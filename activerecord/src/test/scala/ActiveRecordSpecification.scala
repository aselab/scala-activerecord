package com.github.aselab.activerecord

import org.slf4j.LoggerFactory
import ch.qos.logback.classic._

trait DatabaseSpecification extends ActiveRecordSpecification {
  def loglevel: Level = Level.OFF
  def logger(name: String) = LoggerFactory.getLogger(name).asInstanceOf[Logger]
  logger(org.slf4j.Logger.ROOT_LOGGER_NAME).setLevel(loglevel)
  if (System.getProperty("debug") == "true")
    logger("activerecord").setLevel(Level.DEBUG)
}
