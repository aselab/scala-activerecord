# Scala ActiveRecord Play2.1 Plugin

## Usage

### project/Build.scala

Add the following settings in `build.sbt` or `project/Build.scala`

```scala
libraryDependencies ++= Seq(
  "com.github.aselab" %% "scala-activerecord" % "0.2.3",
  "com.github.aselab" %% "scala-activerecord-play2" % "0.2.3",
  jdbc,
  "com.h2database" % "h2" % "1.4.180"  // See Supported databases
)
```

### conf/play.plugins

Add the following settings in `conf/play.plugins`

```
9999:com.github.aselab.activerecord.ActiveRecordPlugin
```

### app/models/Tables.scala

Extend `ActiveRecordTables` with `com.github.aselab.activerecord.PlaySupport`.

```scala
package models

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._

object Tables extends ActiveRecordTables with PlaySupport {
  val models = table[SomeActiveRecordModel]
}
```

### conf/application.conf

```
# Database configuration
# ~~~~~ 
#

# Scala ActiveRecord configurations
db.activerecord.driver=org.h2.Driver
db.activerecord.url="jdbc:h2:mem:play"
db.activerecord.user="sa"
db.activerecord.password=""

# Schema definition class
activerecord.schema=models.Tables
```

### PlayFormSupport(Optional)

`com.github.aselab.activerecord.PlayFormSupport` provides `play.api.data.Form` and several view helpers.

Extends your model's companion objects with PlayFormSupport.

```scala
package models

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._

case class Person(@Required name: String) extends ActiveRecord
object Person extends ActiveRecordCompanion[Person] with PlayFormSupport[Person]
```

In the controllers:

```scala
def create = Action { implicit request =>
  Person.form.bindFromRequest.fold(
    errors => ...,
    person => ...
  )
}

def update(id: Long) = Action { implicit request =>
  Person.find(id) match {
    case Some(person) => Person.form(person).bindFromRequest.fold(
      errors => ...,
      person => ...
    )
    case None => NotFound
  }
}
```
