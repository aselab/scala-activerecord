package com.github.aselab.activerecord

import aliases._
import io._
import inner._
import scala.reflect.ClassTag
import play.api.data._
import play.api.data.Forms._
import play.api.data.format._
import play.api.i18n._
import play.api.templates.Html
import _root_.views.html.{helper => playhelper}
import _root_.views.html.helper._


class ActiveModelFormatter[T <: ActiveModel](
  companion: ActiveModelCompanion[T], source: Option[T]) extends Formatter[T] { 
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

trait PlayFormSupport[T <: ActiveModel] { self: ActiveModelCompanion[T] =>
  lazy val helper = new PlayHelper(self)

  def mapping(source: Option[T] = None) =
    of(new ActiveModelFormatter[T](self, source))

  def form(source: T) = {
    val m = Option(source)
    Form(mapping(m), source.toFormValues, Nil, m)
  }

  def form = Form(mapping(), Map(), Nil, None)
}

class PlayHelper[T <: ActiveModel](companion: ActiveModelCompanion[T]) {
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

  def inputText(field: Field, options: (Symbol, Any)*)(implicit handler: FieldConstructor, lang: play.api.i18n.Lang) =
    playhelper.inputText(field, inputOptions(field, options.toSeq):_*)

  def inputPassword(field: Field, options: (Symbol, Any)*)(implicit handler: FieldConstructor, lang: play.api.i18n.Lang) =
    playhelper.inputPassword(field, inputOptions(field, options.toSeq):_*)

  def select(field: Field, fields: Seq[(String, String)], options: (Symbol, Any)*)(implicit handler: FieldConstructor, lang: play.api.i18n.Lang) =
    playhelper.select(field, fields, inputOptions(field, options.toSeq):_*)

  def textarea(field: Field, options: (Symbol, Any)*)(implicit handler: FieldConstructor, lang: play.api.i18n.Lang) =
    playhelper.textarea(field, inputOptions(field, options.toSeq):_*)

  implicit def fieldConstructor(implicit m: ClassTag[T]) = new FieldConstructor {
    def apply(elements: FieldElements) = {
      val error = if (elements.hasErrors) "error" else ""
      Html(<div class={"control-group %s %s".format(elements.args.get('_class).getOrElse(""), error)} 
        id={elements.args.get('_id).map(_.toString).getOrElse(elements.id + "_field")}>
        <label class="control-label" for={elements.id}>{Config.translator.field(m.runtimeClass, elements.field.name)(elements.lang.toLocale)}</label>
        <div class="controls">
          {xml.Unparsed(elements.input.body)}
          {if (elements.errors.length > 0) {
              <span class="help-inline">{elements.errors(elements.lang).mkString(", ")}</span>
          }}
        </div>
      </div>.toString)
    }
  }
}

