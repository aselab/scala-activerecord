package com.github.aselab.activerecord

import aliases._
import play.api.data._
import play.api.data.Forms._
import play.api.data.format._

class ActiveRecordFormatter[T <: AR](
  companion: ActiveRecordBaseCompanion[_, T], source: Option[T])
  (implicit m: Manifest[T]) extends Formatter[T] {

  def bind(key: String, data: Map[String, String]): Either[Seq[FormError], T] = {
    val m = companion.bind(data)(source.getOrElse(companion.newInstance))
    if (m.validate) {
      Right(m)
    } else {
      Left(m.errors.map(e => FormError(e.key, e.error, e.args.toSeq)).toSeq)
    }
  }

  def unbind(key: String, value: T): Map[String, String] = companion.unbind(value)
}

trait PlayFormSupport[T <: AR] { self: ActiveRecordBaseCompanion[_, T] =>
  def mapping(source: Option[T] = None) =
    of(new ActiveRecordFormatter[T](self, source))

  def form(source: T) = {
    val m = Option(source)
    Form(mapping(m), source.toFormValues, Nil, m)
  }

  def form = Form(mapping(), Map(), Nil, None)
}

