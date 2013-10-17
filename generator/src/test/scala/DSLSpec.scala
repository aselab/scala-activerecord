package com.github.aselab.activerecord.generator

import sbt._
import org.specs2.mutable._
import org.specs2.mock._
import org.specs2.specification._

object DSLSpec extends Specification with BeforeAfterExample with Mockito {
  sequential

  class DSLTester extends DSL {
    val logger = mock[Logger]
    val engine = mock[ScalateTemplateEngine]
  }

  val dsl = new DSLTester
  var workDir: File = _

  def before {
    workDir = IO.createTemporaryDirectory
  }

  def after {
    IO.delete(workDir)
  }

  def createTestFile(data: String) = {
    val file = workDir / "testFile"
    IO.write(file, data)
    file
  }

  "DSL" should {
    "createDirectory" in {
      val dir = workDir / "test"
      dsl.createDirectory(dir) mustEqual "create"
      dir.exists must beTrue

      dsl.createDirectory(dir) mustEqual "exist"
    }

    "createFile" in {
      val content = "line1\nline2\n"
      val file = workDir / "testFile"
      dsl.createFile(file, content) mustEqual "create"
      IO.read(file) mustEqual content

      dsl.createFile(file, content) mustEqual "identical"
      IO.read(file) mustEqual content
    }

    "prependFile" in {
      val content = "line1\nline2\n"
      val file = createTestFile(content)
      dsl.prependFile(file, "prepend") mustEqual "insert"
      IO.read(file) mustEqual "prepend" + content

      dsl.prependFile(file, "prepend") mustEqual "identical"
      IO.read(file) mustEqual "prepend" + content
    }

    "appendFile" in {
      val content = "line1\nline2\n"
      val file = createTestFile(content)
      dsl.appendFile(file, "append") mustEqual "insert"
      IO.read(file) mustEqual content + "append"

      dsl.appendFile(file, "append") mustEqual "identical"
      IO.read(file) mustEqual content + "append"
    }

    "insertFileBefore" in {
      val content = "line1\nline2\n"
      val file = createTestFile(content)
      dsl.insertFileBefore(file, "line2", "insert\n") mustEqual "insert"
      IO.read(file) mustEqual "line1\ninsert\nline2\n"

      dsl.insertFileBefore(file, "line2", "insert\n") mustEqual "identical"
      IO.read(file) mustEqual "line1\ninsert\nline2\n"

      dsl.insertFileBefore(file, "not found pattern", "insert") mustEqual "skip"
    }

    "insertFileAfter" in {
      val content = "line1\nline2\n"
      val file = createTestFile(content)
      dsl.insertFileAfter(file, "line1\n", "insert\n") mustEqual "insert"
      IO.read(file) mustEqual "line1\ninsert\nline2\n"

      dsl.insertFileAfter(file, "line1\n", "insert\n") mustEqual "identical"
      IO.read(file) mustEqual "line1\ninsert\nline2\n"

      dsl.insertFileAfter(file, "not found pattern", "insert") mustEqual "skip"
    }

    "insert into scala source file" in {
      val content = """
      |import com.github.aselab.activerecord._
      |
      |object Tables extends ActiveRecordTables {
      |  lazy val users = table[User]
      |}
      """.stripMargin

      val regex = "ActiveRecordTables[^\\{]*\\{"
      val insert = "\n  lazy val groups = table[Group]"

      val expect = """
      |import com.github.aselab.activerecord._
      |
      |object Tables extends ActiveRecordTables {
      |  lazy val groups = table[Group]
      |  lazy val users = table[User]
      |}
      """.stripMargin

      val file = createTestFile(content)
      dsl.insertFileAfter(file, regex, insert) mustEqual "insert"
      IO.read(file) mustEqual expect
    }
  }
}
