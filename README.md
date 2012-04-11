## Scala ActiveRecord [![Build Status](https://secure.travis-ci.org/aselab/scala-activerecord.png?branch=master)](http://travis-ci.org/aselab/scala-activerecord)

scala-activerecord is an ORM library for Scala.
This library is inspired by ActiveRecord of Ruby on Rails and helps to reduce your code.

## Example

Model implementation:
```scala
import com.github.aselab.activerecord._

case class User(name: String, age: Int) extends ActiveRecord

object User extends ActiveRecordCompanion[User]
```

Schema definition:
```scala
import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._

object Tables extends ActiveRecordTables {
  val userTable = table[User]

  val all = List(userTable)
}
```

ActiveRecord model usage:
```scala
import com.github.aselab.activerecord.dsl._

User("user1", 25).save
User("user2", 18).save
User("user3", 40).save

User.findBy("name", "user1").head //=> User("user1", 25)
User.where(_.age gte 20).toList //=> List(User("user1", 25), User("user3", 40))
User.all.orderBy(_.age desc).toList //=> List(User("user3", 40), User("user1", 25), User("user2", 18))
```

Query DSL is based on [Squeryl](http://squeryl.org/).

Sample project at https://github.com/aselab/scala-activerecord-sample

## License

MIT
