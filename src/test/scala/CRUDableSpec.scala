package com.github.aselab.activerecord

import org.specs2.mutable._

object CRUDableSpec extends Specification {
  def testModel(
    isNew: Boolean = true,
    createResult: Boolean = true,
    updateResult: Boolean = true,
    deleteResult: Boolean = true
  ) = new CRUDable {
    _isNewInstance = isNew
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
    "新規作成 成功時" in {
      val m = testModel()
      val result = m.save()

      "saveの戻り値がtrueであること" in {
        result must beTrue
      }

      "isNewInstanceの値がfalseに変更されること" in {
        m.isNewInstance must beFalse
      }

      "beforeCreate, beforeSave, doCreate, afterCreate, afterSaveメソッドが呼ばれること" in {
        m.calledMethods must equalTo(List("beforeCreate", "beforeSave", "doCreate", "afterCreate", "afterSave"))
      }
    }

    "新規作成 失敗時" in {
      val m = testModel(createResult = false)
      val result = m.save()

      "saveの戻り値がfalseであること" in {
        result must beFalse
      }

      "isNewInstanceの値がtrueのままであること" in {
        m.isNewInstance must beTrue
      }

      "beforeCreate, beforeSave, doCreateメソッドが呼ばれ，afterSaveメソッドが呼ばれないこと" in {
        m.calledMethods must equalTo(List("beforeCreate", "beforeSave", "doCreate"))
      }
    }

    "更新 成功時" in {
      val m = testModel(isNew = false)
      val result = m.save()

      "saveの戻り値がtrueであること" in {
        result must beTrue
      }

      "beforeUpdate, beforeSave, doUpdate, afterUpdate, afterSaveメソッドが呼ばれること" in {
        m.calledMethods must equalTo(List("beforeUpdate", "beforeSave", "doUpdate", "afterUpdate", "afterSave"))
      }
    }

    "更新 失敗時" in {
      val m = testModel(isNew = false, updateResult = false)
      val result = m.save()

      "saveの戻り値がfalseであること" in {
        result must beFalse
      }

      "beforeUpdate, beforeSave, doUpdateメソッドが呼ばれ，afterSaveメソッドが呼ばれないこと" in {
        m.calledMethods must equalTo(List("beforeUpdate", "beforeSave", "doUpdate"))
      }
    }
  }

  "delete()" should {
    "新規インスタンスの時" in {
      val m = testModel()
      val result = m.delete()

      "deleteの戻り値がfalseであること" in {
        result must beFalse
      }

      "どのメソッドも呼ばれないこと" in {
        m.calledMethods must beEmpty
      }
    }

    "削除 成功時" in {
      val m = testModel(isNew = false)
      val result = m.delete()

      "deleteの戻り値がtrueであること" in {
        result must beTrue
      }

      "beforeDelete, doDelete, afterDeleteメソッドが呼ばれること" in {
        m.calledMethods must equalTo(List("beforeDelete", "doDelete", "afterDelete"))
      }

      "isNewInstanceがtrueに変更されること" in {
        m.isNewInstance must beTrue
      }
    }

    "削除 失敗時" in {
      val m = testModel(isNew = false, deleteResult = false)
      val result = m.delete()

      "deleteの戻り値がfalseであること" in {
        result must beFalse
      }

      "beforeDelete, doDeleteメソッドが呼ばれ，afterDeleteメソッドが呼ばれないこと" in {
        m.calledMethods must equalTo(List("beforeDelete", "doDelete"))
      }

      "isNewInstanceがfalseのままであること" in {
        m.isNewInstance must beFalse
      }
    }

  }
}
