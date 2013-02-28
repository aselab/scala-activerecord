# Scala ActiveRecord Scalatra 2.2.0 Plugin

## Usage

### project/Build.scala

Add the following settings in `build.sbt` or `project/Build.scala`

```scala
libraryDependencies ++= Seq(
  "org.scalatra" %% "scalatra" % "2.2.0",
  "com.github.aselab" %% "scala-activerecord" % "0.2-SNAPSHOT",
  "com.github.aselab" %% "scala-activerecord-scalatra" % "0.2-SNAPSHOT",
  "com.h2database" % "h2" % "1.3.170"  // See Supported databases
)

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots")
)
```

### app/models/Tables.scala

with mixin `com.github.aselab.activerecord.ScalatraSupport`.

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

With mixin `com.github.aselab.activerecord.scalatra.DatabaseSupport` to `ScalatraKernel`

```scala
package controllers

import org.scalatra._
import com.github.aselab.activerecord.scalatra._

trait ApplicationController extends ScalatraKernel with DatabaseSupport

class SomeController extends ScalatraServlet with ApplicationController {
  get("/") {
    <h1>Hello, World</h1>
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
