package com.github.aselab.activerecord.inner

import com.github.aselab.activerecord._
import models._

class OptimisticSpec extends DatabaseSpecification with AutoRollback {
  override def beforeAll(): Unit = {
    super.beforeAll()
    OptimisticModel("record1").save()
    OptimisticModel("record2").save()
    ()
  }

  "Optimistic" should {
    "optimistic locking (update)" >> {
      val m1 = OptimisticModel.head
      val m2 = OptimisticModel.head
      val m3 = OptimisticModel.last
      m1.field = "update"
      m1.save()
      m2.field = "other update"
      m2.save() must throwA[StaleObjectException]
      m3.field = "other record update"
      m3.save() must beTrue
    }

    "optimistic locking (delete)" >> {
      val m1 = OptimisticModel.head
      val m2 = OptimisticModel.head
      val m3 = OptimisticModel.last
      m1.field = "update"
      m1.save()
      m2.delete() must throwA[StaleObjectException]
      m3.delete() must beTrue
    }
  }
}

