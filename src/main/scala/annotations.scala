package com.github.aselab.activerecord

import org.squeryl.annotations._

/** アノテーション定義 */
object Annotations {
  import annotation.target._

  /**
   * 無視フィールドアノテーション.
   * フォーム検証対象外のフィールドに付加する．
   */
  type Ignore = annotations.Ignore @field

  /**
   * ユニークアノテーション.
   */
  type Unique = annotations.Unique @field
}
