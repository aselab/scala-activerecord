package org.squeryl

import mojolly.inflector.InflectorImports._

class DummyTable[T](name: String, schema: Schema)(
  implicit m: Manifest[T], ked: KeyedEntityDef[T, _]
) extends Table[T](
  name, m.erasure.asInstanceOf[Class[T]], schema, None, Option(ked)
)
