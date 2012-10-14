package org.squeryl

class DummyTable[T](name: String)(
  implicit m: Manifest[T], ked: KeyedEntityDef[T, _]
) extends Table[T](
  name, m.erasure.asInstanceOf[Class[T]],
  new Schema()(PrimitiveTypeMode), None, Option(ked)
)
