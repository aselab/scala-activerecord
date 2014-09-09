package com.github.aselab.activerecord

import play.api._
import play.api.test._
import com.github.aselab.activerecord._

trait ActiveRecordPlaySpecification extends ActiveRecordSpecification {
  // override application.conf settings
  override def config = super.config ++ Map(
    "db.activerecord.driver" -> "org.h2.Driver",
    "db.activerecord.url" -> ("jdbc:h2:mem:activerecord-test-" + scala.util.Random.nextInt)
  )

  override def beforeAll = {
    Play.start(FakeApplication(additionalConfiguration = config))
    super.beforeAll
  }

  override def afterAll = {
    try {
      super.afterAll
    } finally {
      Play.stop
    }
  }

  override def schema: Seq[ActiveRecordTables] = PlayConfig.loadSchemas
}
