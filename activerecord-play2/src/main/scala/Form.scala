package com.github.aselab.activerecord

import aliases._
import play.api.data._
import play.api.data.Forms._
import play.api.data.format._
import _root_.views.html.{helper => playhelper}

class ActiveRecordFormatter[T <: AR](
  companion: ActiveRecordBaseCompanion[_, T], source: Option[T])
  (implicit m: Manifest[T]) extends Formatter[T] {

  def bind(key: String, data: Map[String, String]): Either[Seq[FormError], T] = {
    val m = companion.bind(data)(source.getOrElse(companion.newInstance))
    if (m.validate) {
      Right(m)
    } else {
      Left(m.formErrors.map(e => FormError(e.key, e.error, e.args.toSeq)).toSeq)
    }
  }

  def unbind(key: String, value: T): Map[String, String] = companion.unbind(value)
}

trait PlayFormSupport[T <: AR] { self: ActiveRecordBaseCompanion[_, T] =>
  lazy val defaultHelper = new PlayHelper(self)
  def helper = defaultHelper

  def mapping(source: Option[T] = None) =
    of(new ActiveRecordFormatter[T](self, source))

  def form(source: T) = {
    val m = Option(source)
    Form(mapping(m), source.toFormValues, Nil, m)
  }

  def form = Form(mapping(), Map(), Nil, None)
}

class PlayHelper[T <: AR](companion: ActiveRecordBaseCompanion[_, T]) {
  protected def inputOptions(field: Field, options: Seq[(Symbol, Any)] = Nil) = {
    val isRequired = options.collectFirst{ case ('required, v) => v }
      .getOrElse(companion.isRequired(field.name))
    (('required -> isRequired) +: options).toMap.toSeq.flatMap {
      case ('required, true) => Seq('_class -> "required")
      case ('required, false) => Nil
      case ('label, v) => Seq('_label ->  v)
      case v => Seq(v)
    }
  }

  def inputText(field: Field, options: (Symbol, Any)*)(implicit lang: play.api.i18n.Lang) =
    playhelper.inputText(field, inputOptions(field, options.toSeq):_*)

  def select(field: Field, fields: Seq[(String, String)], options: (Symbol, Any)*)(implicit lang: play.api.i18n.Lang) =
    playhelper.select(field, fields, inputOptions(field, options.toSeq):_*)

  def textarea(field: Field, options: (Symbol, Any)*)(implicit lang: play.api.i18n.Lang) =
    playhelper.textarea(field, inputOptions(field, options.toSeq):_*)
}

