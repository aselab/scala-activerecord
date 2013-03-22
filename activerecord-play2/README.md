# Scala ActiveRecord Play2.1 Plugin

## Usage

### project/Build.scala

Add the following settings in `project/Build.scala`

```scala
val appDependencies = Seq(
  "com.github.aselab" %% "scala-activerecord" % "0.2.1",
  "com.github.aselab" %% "scala-activerecord-play2" % "0.2.1",
  jdbc,
  "com.h2database" % "h2" % "1.3.170"  // See Supported databases
)

val main = play.Project(appName, appVersion, appDependencies).settings(
  // Add your own project settings here
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases")
  )
)
```

### conf/play.plugins

Add the following settings in `conf/play.plugins`

```
9999:com.github.aselab.activerecord.ActiveRecordPlugin
```

### app/models/Tables.scala

with mixin `com.github.aselab.activerecord.PlaySupport`.

```scala
package models

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._

object Tables extends ActiveRecordTables with PlaySupport {
  val models = table[SomeActiveRecordModel]
}
```

### conf/application.conf (Optional)

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

