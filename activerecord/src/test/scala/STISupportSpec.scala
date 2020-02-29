package com.github.aselab.activerecord

import org.specs2.specification._

import com.github.aselab.activerecord.dsl._
import models._

class STISupportSpec extends DatabaseSpecification with AutoRollback {
  "STI" should {
    "type" >> {
      Student.newInstance._type mustEqual "Student"
      Teacher.newInstance._type mustEqual "Teacher"
    }
  }

  "STISupport" should {
    trait TestData extends Scope {
      val (bob, alice) = (Student("Bob", 20).create, Teacher("Alice", 24).create)
    }

    trait MultipleTestData extends Scope {
      Seq(Student("Bob", 20), Student("John", 22), Student("Hiro", 18)).foreach(_.create)
      Seq(Teacher("Alice", 24), Teacher("Taro", 26), Teacher("Hanako", 28)).foreach(_.create)
    }

    "table" >> {
      Student.table.name mustEqual "people"
      Teacher.table.name mustEqual "people"
    }

    "defaultScope" >> {
      Student.toSql mustEqual PersonView.where(_._type === "Student").toSql
      Teacher.toSql mustEqual PersonView.where(_._type === "Teacher").toSql
    }

    "create" >> new TestData {
      PersonView.size mustEqual 2
      PersonView.toList mustEqual List(Student("Bob", 20), Teacher("Alice", 24))
      PersonView.select(_._type).toList mustEqual List("Student", "Teacher")
      Student.toList mustEqual List(bob)
      Teacher.toList mustEqual List(alice)
    }

    "find" >> new TestData {
      Student.find(bob.id) must beSome(bob)
      Teacher.find(alice.id) must beSome(alice)
      Student.find(alice.id) must beNone
      Teacher.find(bob.id) must beNone
    }

    "delete" >> new TestData {
      Student.delete(bob.id) mustEqual true
      PersonView.count mustEqual 1
      Student.count mustEqual 0
      Teacher.count mustEqual 1
    }

    "forceUpdateAll" >> new MultipleTestData {
      Student.forceUpdateAll(_.age := 19)
      Student.select(_.age).orderBy(_.age).toSeq mustEqual Seq(19, 19, 19)
      Teacher.select(_.age).orderBy(_.age).toSeq mustEqual Seq(24, 26, 28)
    }

    "forceUpdate" >> new MultipleTestData {
      Student.forceUpdate(_.age.~ <= 20)(_.age := 19)
      Student.select(_.age).orderBy(_.age).toSeq mustEqual Seq(19, 19, 22)
      Teacher.select(_.age).orderBy(_.age).toSeq mustEqual Seq(24, 26, 28)
    }

    "forceDelete" >> new MultipleTestData {
      Student.forceDelete(_.age.~ > 19) mustEqual 2
      Student.count mustEqual 1
      Teacher.count mustEqual 3
    }

    "forceDeleteAll" >> new MultipleTestData {
      Student.forceDeleteAll mustEqual 3
      Student.count mustEqual 0
      Teacher.count mustEqual 3
    }
  }
}
