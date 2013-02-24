package com.github.aselab.activerecord.inner

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import org.specs2.specification.Scope

package manytomany {
  case class Foo(name: String) extends ActiveRecord {
    lazy val bars = hasAndBelongsToMany[Bar]
    lazy val many = hasMany[Inter]
    lazy val through = hasManyThrough[Bar, Inter](many)
  }

  case class Bar(name: String) extends ActiveRecord {
    lazy val foos = hasAndBelongsToMany[Foo]
  }

  case class Inter(value: Int) extends ActiveRecord {
    val fooId: Long = 0
    val barId: Long = 0
    lazy val foo = belongsTo[Foo]
    lazy val bar = belongsTo[Bar]
  }

  object Foo extends ActiveRecordCompanion[Foo]
  object Bar extends ActiveRecordCompanion[Bar]
  object Inter extends ActiveRecordCompanion[Inter]

  object Tables extends ActiveRecordTables {
    val foos = table[Foo]
    val bars = table[Bar]
    val inters = table[Inter]
  }

  trait TestData extends Scope {
    val foo = Foo("foo1").create
    val bar = Bar("bar1").create
  }
}

object ManyToManyAssociationSpec extends DatabaseSpecification {
  import manytomany._

  override val config = Map(
    "schema" -> "com.github.aselab.activerecord.inner.manytomany.Tables"
  )

  "HasManyThroughAssociation" should {
    "assign persisted record" >> new TestData {
      val inter = foo.through.assign(bar)
      inter.isPersisted must beFalse
      inter.fooId mustEqual foo.id
      inter.barId mustEqual bar.id
    }

    "assign non-persisted record" >> new TestData {
      val newBar = Bar("bar2")
      foo.through.assign(newBar) must throwA(ActiveRecordException.recordMustBeSaved)
    }

    "assign to non-persisted record" >> new TestData {
      val newFoo = Foo("foo2")
      newFoo.through.assign(bar) must throwA(ActiveRecordException.recordMustBeSaved)
    }

    "associate persisted record" << new TestData {
      val inter = foo.through << bar
      inter.isPersisted must beTrue
      foo.through.toList mustEqual List(bar)
    }

    "implicit conversions" in new TestData {
      val inter = foo.through << bar
      foo.through.where(_.id === bar.id).toList mustEqual List(bar)
    }
  }

  "HasAndBelongsToManyAssociation" should {
    "associate non-persisted record" >> new TestData {
      val newBar = Bar("bar2")
      foo.bars << newBar must throwA(ActiveRecordException.recordMustBeSaved)
    }

    "associate to non-persisted record" >> new TestData {
      val newFoo = Foo("foo2")
      newFoo.bars << bar must throwA(ActiveRecordException.recordMustBeSaved)
    }

    "associate persisted record" >> new TestData {
      foo.bars << bar
      foo.bars.toList mustEqual List(bar)
      bar.foos.toList mustEqual List(foo)
    }

    "implicit conversions" >> new TestData {
      foo.bars << bar
      foo.bars.where(_.id === bar.id).toList mustEqual List(bar)
    }
  }
}
