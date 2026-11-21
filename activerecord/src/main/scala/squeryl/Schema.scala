package org.squeryl

import internals.FieldMapper
import dsl.ast.FieldSelectElement
import java.sql.ResultSet
import internals.ResultSetMapper

import com.github.aselab.activerecord.reflections.ReflectionUtil
import com.github.aselab.activerecord.STI
import collection.mutable.{HashMap, ArrayBuffer}
import scala.reflect.ClassTag

class CustomSchema(implicit override val fieldMapper: FieldMapper) extends Schema {
  override protected def table[T](name: String)(optionalFieldsInfo: Map[String, Class[?]])(
    implicit manifestT: ClassTag[T], ked: OptionalKeyedEntityDef[T, ?]
  ): Table[T] = {
    val typeT = manifestT.runtimeClass.asInstanceOf[Class[T]]
    val t = new CustomTable[T](name, typeT, this, None, ked.keyedEntityDef, Some(optionalFieldsInfo))
    _addTable(t)
    _addTableType(typeT, t)
    t
  }

  // Squeryl 0.11 drops the field types for prefixed tables (it passes None); keep them
  override protected def table[T](name: String, prefix: String)(optionalFieldsInfo: Map[String, Class[?]])(
    implicit manifestT: ClassTag[T], ked: OptionalKeyedEntityDef[T, ?]
  ): Table[T] = {
    val typeT = manifestT.runtimeClass.asInstanceOf[Class[T]]
    val t = new CustomTable[T](name, typeT, this, Some(prefix), ked.keyedEntityDef, Some(optionalFieldsInfo))
    _addTable(t)
    _addTableType(typeT, t)
    t
  }

  private[squeryl] val _stiTableTypes = new HashMap[Class[_], Table[_]]

  override private [squeryl] def _addTableType(typeT: Class[_], t: Table[_]) = {
    if (classOf[STI].isAssignableFrom(typeT)) {
      _stiTableTypes += ((typeT, t))
    }
    super._addTableType(typeT, t)
  }

  override private [squeryl] def _addTable(t: Table[_]): ArrayBuffer[org.squeryl.Table[_]] = {
    if (!tables.exists(_.name == t.name)) {
      return super._addTable(t)
    }
    ArrayBuffer.empty
  }

  /**
   * The root model type of an STI hierarchy: the most general supertype of `typeT`
   * (excluding `typeT` itself and STI) that mixes in STI.
   */
  lazy val baseARType: Class[_] => Option[Class[_]] = (typeT: Class[_]) => {
    val candidates = ReflectionUtil.supertypes(typeT).filter { c =>
      c != classOf[STI] && classOf[STI].isAssignableFrom(c)
    }
    candidates.find(c => !candidates.exists(d => d != c && d.isAssignableFrom(c)))
  }
}

class CustomTable[T](
  _name: String,
  _classOfT: Class[T],
  customSchema: CustomSchema,
  _prefix: Option[String],
  _ked: Option[KeyedEntityDef[T, ?]],
  _optionalFieldsInfo: Option[Map[String, Class[?]]]
) extends Table[T](_name, _classOfT, customSchema, _prefix, _ked, _optionalFieldsInfo) {

  lazy val isSTI = classOf[STI].isAssignableFrom(classOfT)

  private def mapping(arTable: Table[_], resultSet: ResultSet): T = {
    val o = Option(_callbacks.create).getOrElse(arTable._createInstanceOfRowObject)
    val rsmd = resultSet.getMetaData
    val columns = (1 to rsmd.getColumnCount).map(rsmd.getColumnName).map(_.toLowerCase)
    val rm = new ResultSetMapper
    val selectedFieldMetaData = arTable.allFieldsMetaData.filter(fmd =>
      columns.contains(fmd.columnName.toLowerCase))
    for((fmd, i) <- selectedFieldMetaData.zipWithIndex) {
      val jdbcIndex = i + 1
      val fse = new FieldSelectElement(null, fmd, rm)
      fse.prepareColumnMapper(jdbcIndex)
      fse.prepareMapper(jdbcIndex)
    }
    rm.map(o, resultSet)
    _callbacks.afterSelect(o.asInstanceOf[AnyRef]).asInstanceOf[T]
  }

  override private [squeryl]
  def give(resultSetMapper: ResultSetMapper, resultSet: ResultSet) : T = {
    val _type = if (isSTI) {
      try { Some(resultSet.getString("type")) } catch { case e: Exception => None }
    } else {
      None
    }

    if (_type.nonEmpty) {
      val baseType = customSchema.baseARType(classOfT)
      val isExtendTable = (t: Class[_]) => {
        val isExtend = baseType.exists(bt => bt.isAssignableFrom(t))
        val existsClass = _type.exists(_ == t.toString.split("\\.").last)
        isExtend && existsClass
      }
      val arTable = customSchema._stiTableTypes.collect {
        case (typeT, t) if isExtendTable(typeT) => t
      }.head
      mapping(arTable, resultSet)
    } else {
      super.give(resultSetMapper, resultSet)
    }
  }
}
