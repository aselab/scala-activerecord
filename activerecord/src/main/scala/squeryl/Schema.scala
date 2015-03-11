package org.squeryl

import internals.FieldMapper
import scala.reflect.runtime.universe

class CustomSchema(implicit override val fieldMapper: FieldMapper) extends Schema {
  override private [squeryl] def _addTable(t: Table[_]) =
    if (!tables.exists(_.name == t.name)) {
      super._addTable(t)
    }
}
