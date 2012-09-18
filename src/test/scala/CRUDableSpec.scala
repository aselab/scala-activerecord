package com.github.aselab.activerecord

import org.specs2.mutable._

object SaveableSpec extends Specification {
  "Saveable" should {
    "save" in {
      val m = new Saveable {def isNewRecord = true}
      m.save() must beFalse
    }
  }
}

object CRUDableSpec extends Specification {
  def testModel(
    isNew: Boolean = true,
    createResult: Boolean = true,
    updateResult: Boolean = true,
    deleteResult: Boolean = true
  ) = new CRUDable {
    _isNewRecord = isNew
    var calledMethods = List[String]()

    override def beforeSave() {
      calledMethods :+= "beforeSave"
    }

    override def afterSave() {
      calledMethods :+= "afterSave"
    }

    override def beforeCreate() {
      calledMethods :+= "beforeCreate"
    }

    override def afterCreate() {
      calledMethods :+= "afterCreate"
    }

    override def beforeUpdate() {
      calledMethods :+= "beforeUpdate"
    }

    override def afterUpdate() {
      calledMethods :+= "afterUpdate"
    }

    override def beforeDelete() {
      calledMethods :+= "beforeDelete"
    }

    override def afterDelete() {
      calledMethods :+= "afterDelete"
    }

    def doCreate() = {
      calledMethods :+= "doCreate"
      createResult
    }

    def doUpdate() = {
      calledMethods :+= "doUpdate"
      updateResult
    }

    def doDelete() = {
      calledMethods :+= "doDelete"
      deleteResult
    }
  }

  "save()" should {
    "on success to create" in {
      val m = testModel()
      val result = m.save()

      "returns true" in {
        result must beTrue
      }

      "isNewRecord is false" in {
        m.isNewRecord must beFalse
      }

      "should call beforeCreate, beforeSave, doCreate, afterCreate, afterSave" in {
        m.calledMethods must equalTo(List("beforeCreate", "beforeSave", "doCreate", "afterCreate", "afterSave"))
      }
    }

    "on failure to create" in {
      val m = testModel(createResult = false)
      val result = m.save()

      "returns false" in {
        result must beFalse
      }

      "isNewRecord is true" in {
        m.isNewRecord must beTrue
      }

      "should call beforeCreate, beforeSave, doCreate and should not call afterSave" in {
        m.calledMethods must equalTo(List("beforeCreate", "beforeSave", "doCreate"))
      }
    }

    "on success to update" in {
      val m = testModel(isNew = false)
      val result = m.save()

      "returns true" in {
        result must beTrue
      }

      "should call beforeUpdate, beforeSave, doUpdate, afterUpdate, afterSave" in {
        m.calledMethods must equalTo(List("beforeUpdate", "beforeSave", "doUpdate", "afterUpdate", "afterSave"))
      }
    }

    "on failure to update" in {
      val m = testModel(isNew = false, updateResult = false)
      val result = m.save()

      "returns false" in {
        result must beFalse
      }

      "should call beforeUpdate, beforeSave, doUpdate and should not call afterSave" in {
        m.calledMethods must equalTo(List("beforeUpdate", "beforeSave", "doUpdate"))
      }
    }
  }

  "delete()" should {
    "on new record" in {
      val m = testModel()
      val result = m.delete()

      "returns false" in {
        result must beFalse
      }

      "should call nothing" in {
        m.calledMethods must beEmpty
      }
    }

    "on success to delete" in {
      val m = testModel(isNew = false)
      val result = m.delete()

      "returns true" in {
        result must beTrue
      }

      "should call beforeDelete, doDelete, afterDelete" in {
        m.calledMethods must equalTo(List("beforeDelete", "doDelete", "afterDelete"))
      }

      "isNewRecord is true" in {
        m.isNewRecord must beTrue
      }
    }

    "on failure to delete" in {
      val m = testModel(isNew = false, deleteResult = false)
      val result = m.delete()

      "returns false" in {
        result must beFalse
      }

      "should call beforeDelete, doDelete and should not call afterDelete" in {
        m.calledMethods must equalTo(List("beforeDelete", "doDelete"))
      }

      "isNewRecord is false" in {
        m.isNewRecord must beFalse
      }
    }

  }
}
