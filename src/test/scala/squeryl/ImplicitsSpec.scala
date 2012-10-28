package com.github.aselab.activerecord.squeryl

import org.specs2.mutable._
import Implicits._

object ImplicitsSpec extends Specification {
  "anyToOption" should {
    "toOption" in {
      "Option[String]" in {
        val v = Some("a")
        v.toOption[String] mustEqual v
      }

      "Option[Int]" in {
        val v = Some(3)
        v.toOption[Int] mustEqual v
      }

      "None" in {
        None.toOption mustEqual None
      }

      "Int" in {
        3.toOption[Int] mustEqual Some(3)
      }

      "null" in {
        val v: String = null
        v.toOption mustEqual None
      }
    }
  }
}
