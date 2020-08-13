# Scala ActiveRecord [![maven central](https://maven-badges.herokuapp.com/maven-central/com.github.aselab/scala-activerecord_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.github.aselab/scala-activerecord_2.12) [![javadoc](http://javadoc-badge.appspot.com/com.github.aselab/scala-activerecord_2.12.svg?label=scaladoc)](http://javadoc-badge.appspot.com/com.github.aselab/scala-activerecord_2.12/com/github/aselab/activerecord/index.html?javadocio=true)

scala-activerecord is an ORM library for Scala.

This library is inspired by ActiveRecord of Ruby on Rails.
It is designed following the CoC(Convention over Configuration), DRY(Don't Repeat Yourself) principles.

## Minimal example

Model implementation:

```scala
package models

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._

case class Person(name: String, age: Int) extends ActiveRecord

object Person extends ActiveRecordCompanion[Person]
```

Schema definition:

```scala
package models

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._

object Tables extends ActiveRecordTables {
  val people = table[Person]
}
```

ActiveRecord model usage:

```scala
import com.github.aselab.activerecord.dsl._
import models._

object App extends App {
  Tables.initialize
  
  Person("person1", 25).save
  Person("person2", 18).save
  Person("person3", 40).save
  Person("person4", 18).save

  Person.findBy("name", "person1") //=> Some(Person("person1", 25))
  Person.findBy("age", 55) //=> None
  Person.findAllBy("age", 18).toList //=> List(Person("person2", 18), Person("person4", 18))
  Person.where(_.age.~ >= 20).orderBy(_.age desc).toList //=> List(Person("person3", 40), Person("person1", 25))
  
  Tables.cleanup
}
```

Schema and query DSL is based on [Squeryl](http://squeryl.org/).

## Features

* Auto connection management
* Composable query operation
* Callback
* Validation
* Association

## Documents and other resources

* [Wiki](https://github.com/aselab/scala-activerecord/wiki)
* [ScalaDoc](http://javadoc-badge.appspot.com/com.github.aselab/scala-activerecord_2.12/com/github/aselab/activerecord/index.html?javadocio=true)
* [Sample project](https://github.com/aselab/scala-activerecord-sample)
* [CloudBees(CI)](https://aselab.ci.cloudbees.com/job/scala-activerecord/)

## Web framework support

* [Play 2.x plugin](https://github.com/aselab/scala-activerecord/tree/master/play2)
* [Scalatra 2.7.x plugin](https://github.com/aselab/scala-activerecord/tree/master/scalatra)

## License

MIT
