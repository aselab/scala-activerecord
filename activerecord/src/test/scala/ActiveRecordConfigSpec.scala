package com.github.aselab.activerecord

import org.specs2.mutable._
import org.squeryl.adapters._
import com.github.aselab.activerecord.dsl._

package schema1 {
  case class Foo(name: String) extends ActiveRecord

  case class Bar(name: String) extends ActiveRecord

  case class Baz(name: String) extends ActiveRecord

  object Foo extends ActiveRecordCompanion[Foo]
  object Bar extends ActiveRecordCompanion[Bar]
  object Baz extends ActiveRecordCompanion[Baz]

  object Tables extends ActiveRecordTables {
    val foos = table[Foo]
    val bars = table[Bar]
    val bazs = table[Baz]
  }
}

package schema2 {
  case class Hoge(name: String) extends ActiveRecord

  case class Fuga(name: String) extends ActiveRecord

  case class Piyo(name: String) extends ActiveRecord

  object Hoge extends ActiveRecordCompanion[Hoge]
  object Fuga extends ActiveRecordCompanion[Fuga]
  object Piyo extends ActiveRecordCompanion[Piyo]

  object Tables extends ActiveRecordTables {
    val hoges = table[Hoge]
    val fugas = table[Fuga]
    val bazs = table[Piyo]
  }
}

object ActiveRecordConfigSpec extends Specification {
  "DefaultConfig" should {
    "supported database" in {
      def config(driverName: String) = new DefaultConfig(
        models.TestTables,
        overrideSettings = Map("driver" -> driverName)
      )

      "h2 driver" in {
        config("org.h2.Driver").adapter must haveClass[H2Adapter]
      }

      "postgresql driver" in {
        config("org.postgresql.Driver").adapter must haveClass[PostgreSqlAdapter]
      }

      "mysql driver" in {
        config("com.mysql.jdbc.Driver").adapter must beAnInstanceOf[MySQLAdapter]
      }

      "oracle driver" in {
        config("oracle.jdbc.OracleDriver").adapter must haveClass[OracleAdapter]
      }

      "derby driver" in {
        config("org.apache.derby.jdbc.EmbeddedDriver").adapter must haveClass[DerbyAdapter]
      }

      "mssql driver" in {
        config("net.sourceforge.jtds.jdbc.Driver").adapter must haveClass[MSSQLServer]
      }

      "db2 driver" in {
        config("com.ibm.db2.jcc.DB2Driver").adapter must haveClass[DB2Adapter]
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

object MultipleSchemaSpec extends BeforeAfterAllExamples {
  def beforeAll = {
    System.setProperty("run.mode", "test")
    schema1.Tables.initialize(Map("com.github.aselab.activerecord.schema1.Tables.jdbcurl" -> "jdbc:h2:mem:activerecord-test1"))
    schema2.Tables.initialize(Map("com.github.aselab.activerecord.schema2.Tables.jdbcurl" -> "jdbc:h2:mem:activerecord-test2"))
  }

  def afterAll = {
    schema1.Tables.drop
    schema2.Tables.drop
    schema1.Tables.cleanup
    schema2.Tables.cleanup
  }

  "MultipleSchema" should {
    "save" >>  {
      val foo = schema1.Foo("aaa")
      val hoge = schema2.Hoge("aaa")
      val bar = schema1.Bar("aaa")
      foo.save
      hoge.save
      bar.save
      schema1.Foo.toList mustEqual List(foo)
      schema2.Hoge.toList mustEqual List(hoge)
      schema1.Bar.toList mustEqual List(bar)
    }
  }
}
