package com.github.aselab.activerecord.play

import com.github.aselab.activerecord.generator._
import org.specs2.mutable._
import org.specs2.specification._
import java.io._
import scala.sys.process._
import scala.io.Source

object ParserSpec extends Specification with BeforeExample {
  sequential

  var routesFile: File = null

  val routesBody = """# Routes
    |# This file defines all application routes (Higher priority routes first)
    |# ~~~~
    |GET     /user                       controllers.Users.index
    |
    |# Home page
    |GET     /                           controllers.Application.index
    |
    |# Map static resources from the /public folder to the /assets URL path
    |GET     /assets/*file               controllers.Assets.at(path="/public", file)"""

  def before {
    routesFile = File.createTempFile("routes", "")
    val out = new PrintWriter(routesFile)
    routesBody.stripMargin.split("\\r?\\n").foreach(out.println)
    out.close
  }

  "PlayRoute" should {
    "searchInsertPosition" in new withParser {
      parser.searchInsertPosition must beSome(3)
    }

    "alreadyExists (exist)" in new withParser {
      parser.alreadyExists("User") must beTrue
    }

    "alreadyExists (not exist)" in new withParser {
      parser.alreadyExists("Group") must beFalse
    }

    "insert" in new withParser {
      val templateEngine = new ScalateTemplateEngine(null, new File("play20/src/main/resources/templates/"))
      val Some(insertedContents) = parser.insertedContents("Group", templateEngine)
      insertedContents.split("\\r?\\n").toList must containTheSameElementsAs("""# Routes
        |# This file defines all application routes (Higher priority routes first)
        |# ~~~~
        |GET     /groups                       controllers.Groups.index
        |GET     /groups/new                   controllers.Groups.newPage
        |GET     /groups/:id                   controllers.Groups.show(id: Long)
        |POST    /groups                       controllers.Groups.create
        |GET     /groups/:id/edit              controllers.Groups.edit(id: Long)
        |POST    /groups/:id                   controllers.Groups.update(id: Long)
        |DELETE  /groups/:id                   controllers.Groups.delete(id: Long)
        |
        |GET     /user                       controllers.Users.index
        |
        |# Home page
        |GET     /                           controllers.Application.index
        |
        |# Map static resources from the /public folder to the /assets URL path
        |GET     /assets/*file               controllers.Assets.at(path="/public", file)""".stripMargin.split("\\r?\\n").toList)
    }
  }

  trait withParser extends Scope {
    val parser = new Parser.PlayRoute(routesFile)
  }
}
