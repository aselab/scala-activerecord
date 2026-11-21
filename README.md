# Scala ActiveRecord [![maven central](https://maven-badges.herokuapp.com/maven-central/com.github.aselab/scala-activerecord_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.github.aselab/scala-activerecord_2.13) [![javadoc](http://javadoc-badge.appspot.com/com.github.aselab/scala-activerecord_2.13.svg?label=scaladoc)](http://javadoc-badge.appspot.com/com.github.aselab/scala-activerecord_2.13/com/github/aselab/activerecord/index.html?javadocio=true)

scala-activerecord is an ORM library for Scala.

This library is inspired by ActiveRecord of Ruby on Rails.
It is designed following the CoC(Convention over Configuration), DRY(Don't Repeat Yourself) principles.

## Minimal example

* Sample snippet: **[Scastie](https://scastie.scala-lang.org/N5fy1pUZRWWqq8cwLHCgeQ)**

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
  
  Person("person1", 25).save()
  Person("person2", 18).save()
  Person("person3", 40).save()
  Person("person4", 18).save()

  Person.findBy("name", "person1") //=> Some(Person("person1", 25))
  Person.findBy("age", 55) //=> None
  Person.findAllBy("age", 18).toList //=> List(Person("person2", 18), Person("person4", 18))
  Person.where(_.age.~ >= 20).orderBy(_.age.desc).toList //=> List(Person("person3", 40), Person("person1", 25))
  
  Tables.cleanup
}
```

Schema and query DSL is based on [Squeryl](http://squeryl.org/).

## Scala 3

The current master builds for Scala 3 (3.3 LTS); Scala 2.13 users should stay on 0.6.x.

* Building requires JDK 17 or later (sbt 2). The library itself is compiled for Java 11, the minimum of its
  dependencies (HikariCP, nscala-time, json4s).
* Squeryl creates proxies of model classes with cglib. On JDK 9 and later, run your application and tests with
  `--add-opens=java.base/java.lang=ALL-UNNAMED` (with sbt, e.g. `fork := true` and `javaOptions += "..."`).
* The element types of fields such as `Option[Int]` or `List[Long]` are captured at compile time when the model's
  companion object (`ActiveRecordCompanion`, `ActiveModelCompanion`) is defined. For a class without such a companion,
  call `com.github.aselab.activerecord.reflections.FieldTypes.register[MyClass]` before using it.
* Field names and value types passed to `newInstance`, `assign`, `find`, `findBy`, `findAllBy` and `findByOrCreate`
  are still checked at compile time. They must be written out at the call site (string literals); use the `unsafe*`
  variants for dynamic names.
* Squeryl on Scala 3 cannot infer the result type of arithmetic on columns of different types, such as
  `sum(item.price) + avg(item.price)`; pass the result factory explicitly:
  `sum(item.price).+(avg(item.price))(optionFloatTEF)`. Aggregates (`max`, `min`, `sum`, `avg`) work as before.
* scala-activerecord-specs is built on specs2 5 (5.9.1, the latest release that supports Scala 3.3):
  * `ActiveRecordSpecification` runs its examples sequentially. Override `beforeAll()` and `afterAll()`
    (methods, now with `()`) to add set-up and tear-down; call `super` to keep the schema handling.
  * `AutoRollback` rolls back each example with `AroundEach`.
  * The specs2 `===` matcher is disabled, as it would otherwise shadow Squeryl's `===` in queries;
    use `must beEqualTo(...)` instead. specs2 5 removed `mustEqual`, `must have size` and `Scope`.
* The tests use H2 2.x, where `value` is a keyword; add `;NON_KEYWORDS=VALUE` to the JDBC URL if a model has a
  field named `value`.
* The Play 2 and Scalatra modules have not been migrated to Scala 3 yet.

## Features

* Auto connection management
* Composable query operation
* Callback
* Validation
* Association

## Documents and other resources

* [Wiki](https://github.com/aselab/scala-activerecord/wiki)
* [Documentation(WIP) 日本語/English](https://aselab.github.io/docs/scala-activerecord/)
* [ScalaDoc](http://javadoc-badge.appspot.com/com.github.aselab/scala-activerecord_2.13/com/github/aselab/activerecord/index.html?javadocio=true)
* [Sample project](https://github.com/aselab/scala-activerecord-sample)

## Web framework support

* [Play 2.x plugin](https://github.com/aselab/scala-activerecord/tree/master/play2)
* [Scalatra 2.7.x plugin](https://github.com/aselab/scala-activerecord/tree/master/scalatra)

## License

MIT
