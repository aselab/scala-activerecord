# Scala ActiveRecord Play2.6 Plugin

## Sample code

https://github.com/aselab/scala-activerecord-sample/tree/master/play2x

## Usage

### project/Build.scala

Add the following settings in `build.sbt` or `project/Build.scala`

```scala
libraryDependencies ++= Seq(
  "com.github.aselab" %% "scala-activerecord" % "0.4.0-SNAPSHOT",
  "com.github.aselab" %% "scala-activerecord-play2" % "0.4.0-SNAPSHOT",
  jdbc,
  "com.h2database" % "h2" % "1.4.196"  // See Supported databases
)
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
schema.models.Tables=activerecord

play.modules.enabled += "com.github.aselab.activerecord.ActiveRecordPlayModule"
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

## Multiple database support (optional)

### Sample code

https://github.com/aselab/scala-activerecord-sample/tree/master/multiple-schema-app

If the schema, `models.Tables1` and `models.Tables2`, such as the following are defined:

```scala
package models

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._

object Tables1 extends ActiveRecordTables with PlaySupport {
  val users = table[User]
}

object Tables2 extends ActiveRecordTables with PlaySupport {
  val groups = table[Group]
}
```

Configuration is as follows:

#### conf/application.conf

```
# Database configuration
# ~~~~~ 
#

# Scala ActiveRecord configurations
db.schema1.driver=org.h2.Driver
db.schema1.url="jdbc:h2:mem:db1"
db.schema1.user="sa"
db.schema1.password=""

db.schema2.driver=org.h2.Driver
db.schema2.url="jdbc:h2:mem:db2"
db.schema2.user="sa"
db.schema2.password=""

# Schema definition class
schema.models.Tables1=schema1
schema.models.Tables2=schema2

play.modules.enabled += "com.github.aselab.activerecord.ActiveRecordPlayModule"
```

