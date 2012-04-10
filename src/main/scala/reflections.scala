package com.github.aselab.activerecord

import java.lang.annotation.Annotation
import java.lang.reflect.Field
import annotations._

/**
 * フィールド情報モデル
 * @param name フィールド名
 * @param fieldType フィールドタイプ
 * @param isOption Option型かどうか
 * @param isSeq シーケンスかどうか
 * @param annotations アノテーションリスト
 */
case class FieldInfo(
  name: String, fieldType: Class[_],
  isOption: Boolean, isSeq: Boolean,
  annotations: Seq[Annotation] = Nil
) {
  private lazy val annotationMap = annotations.map {
    a => (a.annotationType.getSimpleName, a)
  }.toMap

  /** Ignoreフィールドかどうか */
  lazy val ignored = annotationMap.isDefinedAt("Ignore")
  /** Uniqueフィールドかどうか */
  lazy val unique = annotationMap.isDefinedAt("Unique")
}

/** フィールド情報オブジェクト */
object FieldInfo {
  def apply(name: String, value: Any, field: Option[Field]): FieldInfo = value match {
    case Some(v) => apply(name, v, None).copy(isOption = true)
    case None => throw ConventionException.optionValueMustBeSome

    case l: Traversable[_] => l.toSeq match {
      case Seq(v, _*) => apply(name, v, None).copy(isSeq = true)
      case Nil => throw ConventionException.traversableValueMustNotBeNil
    }

    case v: Any => FieldInfo(name, v.getClass, false, false)
    case v =>
      val fieldType = field.map(_.getType).getOrElse(
        throw ConventionException.cannotDetectType(v))
      FieldInfo(name, fieldType, false, false)
  }

  def apply(name: String, value: Any): FieldInfo = apply(name, value, None)

  def apply(field: Field, value: Any): FieldInfo =
    apply(field.getName, value, Some(field)).copy(annotations = field.getAnnotations.toSeq)
}

/** リフレクション用ユーティリティ */
trait ReflectionUtil {
  /**
   * クラスからコンパニオンオブジェクトを返す.
   * @param className クラス名
   */
  def classToCompanion(className: String): Any = {
    val cc = Class.forName(className + "$")
    cc.getField("MODULE$").get(cc)
  }

  /**
   * クラスからコンパニオンオブジェクトを返す.
   * @param c クラス
   */
  def classToCompanion(c: Class[_]): Any = classToCompanion(c.getName)

  /**
   * クラスからコンパニオンオブジェクトを返す.
   * @param c 任意のオブジェクト
   */
  def companionToClass(c: Any) = Class.forName(c.getClass.getName.dropRight(1))

  /**
   * 任意のオブジェクトのフィールド値をリフレクションで取得/変更できるようにする
   * 暗黙変換メソッド.
   */
  implicit def toReflectable(o: Any) = new {
    val c = o.getClass

    /**
     * 値を取得する.
     * @param name フィールド名
     */
    def getValue[T](name: String) = c.getMethod(name).invoke(o).asInstanceOf[T]

    /**
     * 値を設定する.
     * @param name フィールド名
     * @param value 設定する値
     */
    def setValue(name: String, value: Any) = {
      val f = c.getDeclaredField(name)
      f.setAccessible(true)
      f.set(o, value)
    }
  }
}

/** リフレクション用ユーティリティ */
object ReflectionUtil extends ReflectionUtil
