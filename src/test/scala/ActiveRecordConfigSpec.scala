package com.github.aselab.activerecord

import org.specs2.mutable._
import org.squeryl.adapters._

object ActiveRecordConfigSpec extends Specification {
  "DefaultConfig" should {
    "supported database" in {
      def config(driverName: String) = new DefaultConfig(
        overrideSettings = Map("driver" -> driverName)
      )

      "h2 driver" in {
        config("org.h2.Driver").adapter must haveClass[H2Adapter]
      }

      "postgresql driver" in {
        config("org.postgresql.Driver").adapter must haveClass[PostgreSqlAdapter]
      }

      "mysql driver" in {
        config("com.mysql.jdbc.Driver").adapter must haveClass[MySQLAdapter]
      }

      "oracle driver" in {
        config("oracle.jdbc.OracleDriver").adapter must haveClass[OracleAdapter]
      }

      "derby driver" in {
        config("org.apache.derby.jdbc.EmbeddedDriver").adapter must haveClass[DerbyAdapter]
      }

      "unsupported driver" in {
        config("not.supported.Driver").adapter must throwA(
          ActiveRecordException.unsupportedDriver("not.supported.Driver"))
      }

      "unresolved driver" in {
        config("oracle.jdbc.OracleDriver").pool must throwA(
          ActiveRecordException.missingDriver("oracle.jdbc.OracleDriver")
        )
      }
    }
  }
}
