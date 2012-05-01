package com.github.aselab.activerecord.experimental

import com.github.aselab.activerecord._

trait Serializable { this: ActiveRecord => 
  import ReflectionUtil._

  def toMap(implicit excludeRelation: Boolean = false): Map[String, Any] = {
    def relationMap(o: Any) = o.asInstanceOf[Serializable].toMap(true)
    _companion.formatFields.flatMap { f =>
      val name = f.getName
      (this.getValue[Any](name) match {
        case r: RecordRelation if excludeRelation => None
        case r: ActiveRecordOneToMany[_] => Some(r.toList.map(relationMap))
        case r: ActiveRecordManyToOne[_] => r.one.map(relationMap)
        case r: ActiveRecordManyToMany[_, _] => Some(r.toList.map(relationMap))
        case v: Option[_] => v
        case v => Some(v)
      }).map(name -> _)
    }.toMap
  }

}
