## Scala ActiveRecord [![Build Status](https://aselab.ci.cloudbees.com/job/scala-activerecord/badge/icon)](https://aselab.ci.cloudbees.com/job/scala-activerecord/)

scala-activerecord is an ORM library for Scala.

This library is inspired by ActiveRecord of Ruby on Rails and helps to reduce your code.

## Minimal example

Model implementation:

```scala
package models

import com.github.aselab.activerecord._

case class Person(name: String, age: Int) extends ActiveRecord

object Person extends ActiveRecordCompanion[Person]
```

Schema definition:

```scala
package models

import com.github.aselab.activerecord._

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

## Documents and other resources

* [Wiki](https://github.com/aselab/scala-activerecord/wiki)
* [ScalaDoc](https://aselab.ci.cloudbees.com/job/scala-activerecord/javadoc/)
* [Sample project](https://github.com/aselab/scala-activerecord-sample)

## License

MIT
