package com.github.aselab.activerecord

import com.github.aselab.activerecord.dsl._
import models._

class ActiveRecordTablesSpec extends DatabaseSpecification {
  override def beforeAll() = {
    super.beforeAll()
    TestTables.createTestData
  }

  "session" should {
    "rollback throws exception when not startTransaction" >> {
      TestTables.rollback must throwA[ActiveRecordException]
    }

    "withRollback" >> {
      val users: List[User] = User.all.toList
      TestTables.withRollback {
        User("testuser").save()
        User("testuser2").save()
        User("testuser3").save()
        User.all.toList must not equalTo(users)
      }
      User.all.toList mustEqual users
    }

    "startTransaction and rollback" >> {
      TestTables.startTransaction
      val users: List[User] = User.all.toList
      User("testuser").save()
      User("testuser2").save()
      User("testuser3").save()
      TestTables.rollback
      User.all.toList mustEqual users
    }
  }

  "database schema" should {
    def ddl = {
      var lines = List.empty[String]
      TestTables.transaction { TestTables.printDdl(lines +:= _) }
      lines.mkString("\n")
    }

    "except @Transient column" >> {
      ddl must not matching("""(?ms).* transient_field .*""".r)
    }

    "be able to change column name by @Column" >> {
      ddl must not matching("""(?ms).* column_field .*""".r)
      ddl must matching("""(?ms).* columnName .*""".r)
    }

    "create unique index by @Unique" >> {
      ddl must matching("""(?ms).*create unique index \w+ on annotation_models \(unique_field\);.*""".r)
    }

    "except confirmation column" >> {
      ddl must not matching("""(?ms).* confirmation_field_confirmation .*""".r)
    }

    "except renamed confirmation column" >> {
      ddl must not matching("""(?ms).* confirmation_name .*""".r)
    }

  }
}
