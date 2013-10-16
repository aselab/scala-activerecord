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

  /*val content = """
  |import com.github.aselab.activerecord._
  |
  |object Tables extends ActiveRecordTables {
  |  lazy val users = 
  |}
  """*/
  "DSL" should {
    "createDirectory" in {
      val dir = workDir / "test"
      dsl.createDirectory(dir)
      dir.exists must beTrue
    }

    "prependFile" in {
      val content = "line1\nline2\n"
      val file = createTestFile(content)
      dsl.prependFile(file, "prepend")
      IO.read(file) mustEqual "prepend" + content
    }

    "appendFile" in {
      val content = "line1\nline2\n"
      val file = createTestFile(content)
      dsl.appendFile(file, "append")
      IO.read(file) mustEqual content + "append"
    }

    "insertFileBefore" in {
      val content = "line1\nline2\n"
      val file = createTestFile(content)
      dsl.insertFileBefore(file, "line2", "insert\n")
      IO.read(file) mustEqual "line1\ninsert\nline2\n"
    }

    "insertFileAfter" in {
      val content = "line1\nline2\n"
      val file = createTestFile(content)
      dsl.insertFileAfter(file, "line1\n", "insert\n")
      IO.read(file) mustEqual "line1\ninsert\nline2\n"
    }
  }
}
