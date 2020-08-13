# Scala ActiveRecord Scalatra 2.7.x Plugin

## Sample code

https://github.com/aselab/scala-activerecord-sample/tree/master/scalatra

## Usage

### project/Build.scala

Add the following settings in `build.sbt` or `project/Build.scala`

```scala
libraryDependencies ++= Seq(
  "org.scalatra" %% "scalatra" % "2.7.0",
  "com.github.aselab" %% "scala-activerecord" % "0.6.0",
  "com.github.aselab" %% "scala-activerecord-scalatra" % "0.6.0",
  "com.h2database" % "h2" % "1.4.200"  // See Supported databases
)
```

### app/models/Tables.scala

Extend `ActiveRecordTables` with `com.github.aselab.activerecord.ScalatraSupport`.

```scala
package models

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import com.github.aselab.activerecord.scalatra._

object Tables extends ActiveRecordTables with ScalatraSupport {
  val models = table[SomeActiveRecordModel]
}
```

### DatabaseSupport for controller

Extend `ScalatraKernel` with `com.github.aselab.activerecord.scalatra.DatabaseSupport`

```scala
package controllers

import org.scalatra._
import com.github.aselab.activerecord.scalatra._

trait ApplicationController extends ScalatraKernel with DatabaseSupport

class SomeController extends ScalatraServlet with ApplicationController {
  get("/") {
    <h1>{"record count: " + SomeActiveRecordModel.count}</h1>
  }
}
```

### ActiveRecordLifeCycle (Optional)

Database initialize and cleanup support.
Add the following settings in `src/main/scala/ScalatraBootstrap.scala`

```scala
import org.scalatra.LifeCycle
import com.github.aselab.activerecord.scalatra._
import javax.servlet.ServletContext
import controllers._

class ScalatraBootstrap extends ActiveRecordLifeCycle {
  override def init(context: ServletContext) {
    super.init(context)
    context mount (new SomeController, "/some/*")
  }
}
```
