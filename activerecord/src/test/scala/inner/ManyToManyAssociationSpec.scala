package com.github.aselab.activerecord.inner

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import org.specs2.specification.Scope

package manytomany {
  case class Foo(name: String) extends ActiveRecord {
    lazy val bars = hasAndBelongsToMany[Bar]
    lazy val many = hasMany[Inter]
    lazy val through = hasManyThrough[Bar, Inter](many)
    lazy val throughOption = hasManyThrough[Baz, Inter](many)
  }

  case class Bar(name: String) extends ActiveRecord {
    lazy val foos = hasAndBelongsToMany[Foo]
  }

  case class Baz(name: String) extends ActiveRecord

  case class Inter(value: Int) extends ActiveRecord {
    val fooId: Long = 0
    val barId: Long = 0
    val bazId: Option[Long] = None
    lazy val foo = belongsTo[Foo]
    lazy val bar = belongsTo[Bar]
    lazy val baz = belongsTo[Baz]
  }

  object Foo extends ActiveRecordCompanion[Foo]
  object Bar extends ActiveRecordCompanion[Bar]
  object Baz extends ActiveRecordCompanion[Baz]
  object Inter extends ActiveRecordCompanion[Inter]

  object Tables extends ActiveRecordTables {
    val foos = table[Foo]
    val bars = table[Bar]
    val bazs = table[Baz]
    val inters = table[Inter]
  }

  trait TestData extends Scope {
    val foo = Foo("foo1").create
    val bar = Bar("bar1").create
    val baz = Baz("baz1").create
  }
}

object ManyToManyAssociationSpec extends DatabaseSpecification {
  import manytomany._

  override def schema = Tables

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

    "associate persisted record" >> new TestData {
      val inter = foo.through << bar
      inter.isPersisted must beTrue
      foo.through.toList mustEqual List(bar)
    }

    "remove" >> new TestData {
      val baz2 = Baz("baz2").create
      val inter = foo.throughOption << baz
      val inter2 = foo.throughOption << baz2
      val removed = foo.throughOption.remove(baz)
      removed must beSome(baz)
      foo.throughOption.toList mustEqual List(baz2)
      Inter.find(inter.id).map(_.bazId) must beSome(None)
    }

    "remove with not null constraint" >> new TestData {
      val inter = foo.through << bar
      foo.through.remove(bar) must throwA(ActiveRecordException.notNullConstraint("barId"))
    }

    "removeAll" >> new TestData {
      val inter = foo.throughOption << baz
      val removed = foo.throughOption.removeAll
      removed mustEqual List(baz)
      foo.throughOption.toList must beEmpty
      foo.throughOption.reload must beEmpty
      Inter.find(inter.id).map(_.bazId) must beSome(None)
    }

    "removeAll with not null constraint" >> new TestData {
      val inter = foo.through << bar
      foo.through.removeAll must throwA(ActiveRecordException.notNullConstraint("barId"))
    }

    "deleteAll" >> new TestData {
      val inter = foo.throughOption << baz
      val deleted = foo.throughOption.deleteAll
      deleted mustEqual List(baz)
      foo.throughOption.toList must beEmpty
      Inter.find(inter.id).map(_.bazId) must beSome(None)
    }

    "deleteAll with not null constraint" >> new TestData {
      val inter = foo.through << bar
      val deleted = foo.through.deleteAll
      deleted mustEqual List(bar)
      foo.through.toList must beEmpty
      Inter.find(inter.id) must beNone
    }

    "append records" >> new TestData {
      val bar2 = Bar("bar2").create
      val bar3 = Bar("bar3").create
      val inter = foo.through << bar
      foo.through ++= Seq(bar2, bar3)
      foo.through.toList must contain(exactly(bar, bar2, bar3))
    }

    "replace records" >> new TestData {
      val baz2 = Baz("baz2").create
      val baz3 = Baz("baz3").create
      val inter = foo.throughOption << baz
      foo.throughOption := List(baz2, baz3)
      Inter.find(inter.id).map(_.bazId) must beSome(None)
      foo.throughOption.toList mustEqual List(baz2, baz3)
    }

    "replace records with not null constraint" >> new TestData {
      val bar2 = Bar("bar2").create
      val bar3 = Bar("bar3").create
      val inter = foo.through << bar
      foo.through := List(bar2, bar3)
      Inter.find(inter.id) must beNone
      foo.through.toList mustEqual List(bar2, bar3)
    }

    "implicit conversions" in new TestData {
      val inter = foo.through << bar
      foo.through.where(_.id === bar.id).toList mustEqual List(bar)
    }
  }

  "HasAndBelongsToManyAssociation" should {
    "associate non-persisted record" >> new TestData {
      val newBar = Bar("bar2")
      foo.bars << newBar
      newBar.isPersisted must beTrue
      foo.bars.toList mustEqual List(newBar)
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

    "remove" >> new TestData {
      val bar2 = Bar("bar2").create
      foo.bars := List(bar, bar2)
      foo.bars.remove(bar) must beSome(bar)
      foo.bars.remove(bar) must beNone
      foo.bars.toList mustEqual List(bar2)
      foo.bars.reload mustEqual List(bar2)
      Bar.find(bar.id) must beSome
    }

    "removeAll" >> new TestData {
      foo.bars << bar
      foo.bars.removeAll mustEqual List(bar)
      foo.bars.toList must beEmpty
      foo.bars.reload must beEmpty
      Bar.find(bar.id) must beSome
    }

    "deleteAll" >> new TestData {
      foo.bars << bar
      foo.bars.deleteAll mustEqual List(bar)
      foo.bars.toList must beEmpty
      Bar.find(bar.id) must beNone
    }

    "append records" >> new TestData {
      val bar2 = Bar("bar2").create
      val bar3 = Bar("bar3").create
      foo.bars << bar
      foo.bars ++= Seq(bar2, bar3)
      foo.bars.toList must contain(exactly(bar, bar2, bar3))
    }

    "replace records" >> new TestData {
      val bar2 = Bar("bar2").create
      val bar3 = Bar("bar3").create
      foo.bars << bar
      foo.bars := List(bar2, bar3)
      foo.bars.toList mustEqual List(bar2, bar3)
      foo.bars.reload mustEqual List(bar2, bar3)
      Bar.find(bar.id) must beSome
    }

    "implicit conversions" >> new TestData {
      foo.bars << bar
      foo.bars.where(_.id === bar.id).toList mustEqual List(bar)
    }
  }
}
