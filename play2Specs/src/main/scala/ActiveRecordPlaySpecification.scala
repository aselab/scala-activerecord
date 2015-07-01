package com.github.aselab.activerecord

import play.api._
import play.api.test._

trait ActiveRecordPlaySpecification extends ActiveRecordSpecification {
  var app: Application = _

  // override application.conf settings
  override def config = super.config ++ Map(
    "db.activerecord.driver" -> "org.h2.Driver",
    "db.activerecord.url" -> ("jdbc:h2:mem:activerecord-test-" + scala.util.Random.nextInt)
  )

  override def beforeAll = {
    app = FakeApplication(additionalConfiguration = config)
    Play.start(app)
    super.beforeAll
  }

  override def afterAll = {
    try {
      super.afterAll
    } finally {
      app.stop
    }
  }

  override def schema: Seq[ActiveRecordTables] = PlayConfig.loadSchemas
}
