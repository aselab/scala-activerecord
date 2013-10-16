package com.github.aselab.activerecord.scalatra

import org.scalatra._
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import com.github.aselab.activerecord.dsl

trait DatabaseSupport extends Handler {
  abstract override def handle(req: HttpServletRequest, res: HttpServletResponse) {
    dsl.transaction {
      super.handle(req, res)
    }
  }
}

