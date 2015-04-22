package org.squeryl

import internals.{FieldMapper, FieldMetaData, PosoMetaData}
import dsl.ast.FieldSelectElement
import java.sql.ResultSet
import internals.ResultSetMapper

import com.github.aselab.activerecord.reflections.ReflectionUtil
import com.github.aselab.activerecord.STI
import collection.mutable.HashMap

class CustomSchema(implicit override val fieldMapper: FieldMapper) extends Schema {
  override protected def table[T](name: String)(implicit manifestT: Manifest[T], ked: OptionalKeyedEntityDef[T,_]): Table[T] = {
    val typeT = manifestT.runtimeClass.asInstanceOf[Class[T]]
    val t = new CustomTable[T](name, typeT, this, None, ked.keyedEntityDef)
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

  override private [squeryl] def _addTable(t: Table[_]) =
    if (!tables.exists(_.name == t.name)) {
      super._addTable(t)
    }

  lazy val parentTypes = {
    val parentTypeSig = ReflectionUtil.typeSig(classOf[STI])
    (parentTypeSig, parentTypeSig.typeSymbol)
  }

  lazy val baseARType = (typeT: Class[_]) => {
    val typeTSig = ReflectionUtil.typeSig(typeT)
    val (parentTypeSig, parentType) = parentTypes
    typeTSig.baseClasses.tail.reverse.find { c =>
      ReflectionUtil.isExtend(c, parentType) && !(c.typeSignature =:= parentTypeSig)
    }
  }
}

class CustomTable[T](
  _name: String,
  override private[squeryl] val classOfT: Class[T],
  schema: CustomSchema,
  _prefix: Option[String],
  override val ked: Option[KeyedEntityDef[T,_]]
) extends Table[T](_name, classOfT, schema, _prefix, ked) {

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
      val baseType = schema.baseARType(classOfT)
      val isExtendTable = (t: Class[_]) => {
        val isExtend = baseType.exists(bt => ReflectionUtil.isExtend(t, bt))
        val existsClass = _type.exists(_ == t.toString.split("\\.").last)
        isExtend && existsClass
      }
      val arTable = schema._stiTableTypes.collect {
        case (typeT, t) if isExtendTable(typeT) => t
      }.head
      mapping(arTable, resultSet)
    } else {
      super.give(resultSetMapper, resultSet)
    }
  }
}
