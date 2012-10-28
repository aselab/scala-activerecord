package com.github.aselab.activerecord

import java.lang.annotation.Annotation
import java.lang.reflect.{Field, ParameterizedType}
import scala.reflect.Manifest
import scala.util.control.Exception._
import annotations._
import scala.tools.scalap.scalax.rules.scalasig._

class ClassInfo[T <: AnyRef](clazz: Class[T]) {
  import ClassInfo._

  val name = clazz.getName

  lazy val factory: () => T = {
    if (ReflectionUtil.isSeq(clazz)) {
      factories.nilHandler
    } else {
      getFactory(clazz)
    }
  }.asInstanceOf[() => T]

  lazy val fields: List[java.lang.reflect.Field] = {
    clazz.getDeclaredFields.filterNot {f =>
      f.isAnnotationPresent(classOf[annotations.Ignore]) ||
      classOf[RecordRelation].isAssignableFrom(f.getType) ||
      f.getName.contains("$")
    }.toList
  }

  lazy val fieldInfo: Map[String, FieldInfo] = {
    fields.map { f => (f.getName, FieldInfo(f, this)) }.toMap
  }

  lazy val scalaSigInfo = ScalaSigInfo(clazz)
}

object ClassInfo {
  private val cache = collection.mutable.Map[Class[_], ClassInfo[_]]()

  def apply[T <: AnyRef](clazz: Class[T]): ClassInfo[T] = cache.getOrElseUpdate(
    clazz, new ClassInfo(clazz)).asInstanceOf[ClassInfo[T]]

  lazy val factories = new PrimitiveHandler[() => AnyRef] {
    val stringHandler = () => ""
    val booleanHandler = () => new java.lang.Boolean(false)
    val intHandler = () => new java.lang.Integer(0)
    val longHandler = () => new java.lang.Long(0)
    val floatHandler = () => new java.lang.Float(0)
    val doubleHandler = () => new java.lang.Double(0.0)
    val dateHandler = () => new java.util.Date(0)
    val timestampHandler = () => new java.sql.Timestamp(0)
    val uuidHandler = () => new java.util.UUID(0, 0)
    val bigDecimalHandler = () => BigDecimal(0)
    val nilHandler = () => Nil.asInstanceOf[AnyRef]

    registrations ++= Seq(
      (classOf[Option[_]], () => None),
      (classOf[Array[Byte]], () => Array.empty[Byte])
    )
  }

  def getFactory(clazz: Class[_]): () => AnyRef = factories.getOrRegister(clazz, {
    clazz.getConstructors.map(
      c => (c, c.getParameterTypes.toSeq)
    ).sortBy(_._2.size).toStream.flatMap {
      case (const, params) => allCatch.opt {
        // test creation parameters
        val facts = params.map(c =>
          apply(c.asInstanceOf[Class[AnyRef]]).factory)
        facts.foreach(_.apply)

        () => try {
          const.newInstance(facts.map(_.apply):_*).asInstanceOf[AnyRef]
        } catch {
          case e => throw ActiveRecordException.cannotCreateInstance(
            clazz.getName, e.getMessage)
        }
      }
    }.headOption.getOrElse(throw ActiveRecordException.cannotCreateInstance(
      clazz.getName, "No usable constructor is found. It is recommended to implement default constructor.")
    )
  })
}

case class FieldInfo(
  name: String, fieldType: Class[_],
  isOption: Boolean, isSeq: Boolean,
  annotations: Seq[Annotation] = Nil
) {
  private lazy val annotationMap = annotations.map {
    a => (a.annotationType.getSimpleName, a)
  }.toMap

  lazy val required = annotationMap.isDefinedAt("Required")
  lazy val ignored = annotationMap.isDefinedAt("Ignore")
  lazy val unique = annotationMap.isDefinedAt("Unique")

  def is[T](implicit m: Manifest[T]): Boolean = fieldType == m.erasure
}

object FieldInfo {
  import ReflectionUtil._

  def apply(field: Field, classInfo: ClassInfo[_]): FieldInfo = {
    def notSupported: Nothing = throw ActiveRecordException.unsupportedType(
      classInfo.name + "#" + field.getName
    )

    def genericType: Class[_] = getGenericType(field) match {
      case c if c == classOf[Object] =>
        // detect generic primitive type from ScalaSig
        classInfo.scalaSigInfo.genericTypes.getOrElse(
          field.getName, notSupported
        )

      case c => c
    }

    def fieldInfo(clazz: Class[_]): FieldInfo = clazz match {
      case c if isOption(c) =>
        fieldInfo(genericType).copy(isOption = true)
      case c if isSeq(c) =>
        fieldInfo(genericType).copy(isSeq = true)
      case c if support.allClasses(c.getClassLoader).isDefinedAt(c.getName) =>
        FieldInfo(field.getName, c, false, false, field.getAnnotations.toSeq)
      case c => notSupported
    }

    fieldInfo(field.getType)
  }
}

case class ScalaSigInfo(clazz: Class[_]) {
  def error: Nothing = throw ActiveRecordException.scalaSig(clazz)

  val scalaSig = {
    def find(c: Class[_]): Option[ScalaSig] =
      ScalaSigParser.parse(c).orElse(find(c.getDeclaringClass))
    find(clazz).getOrElse(error)
  }

  val classSymbol = scalaSig.symbols.toIterator.collect {
    case symbol: ClassSymbol if !symbol.isModule => symbol
  }.find(_.name == clazz.getSimpleName).getOrElse(error)

  lazy val genericTypes: Map[String, Class[_]] = classSymbol.children.collect {
    case m: MethodSymbol if m.isLocal => (m.name.trim, m.infoType)
  }.collect {
    case (name, TypeRefType(_, symbol, Seq(TypeRefType(_, s, Nil))))
      if support.primitiveClasses.isDefinedAt(s.path) =>
        (name, support.primitiveClasses(s.path))
  }.toMap
}

trait ReflectionUtil {
  def defaultLoader: ClassLoader = Thread.currentThread.getContextClassLoader

  def loadClass(name: String)(
    implicit classLoader: ClassLoader = defaultLoader
  ): Class[_] = classLoader.loadClass(name)

  /**
   * returns companion object from class name
   * @param className class name
   */
  def classToCompanion(className: String)(
    implicit classLoader: ClassLoader = defaultLoader
  ): Any = {
    val cc = loadClass(className + "$")
    cc.getField("MODULE$").get(cc)
  }

  /**
   * returns companion object from class
   * @param c class
   */
  def classToCompanion(c: Class[_]): Any =
    classToCompanion(c.getName)(c.getClassLoader)

  /**
   * returns corresponding class from companion object
   * @param o companion object
   */
  def companionToClass(o: Any): Class[_] = {
    val c = o.getClass
    loadClass(c.getName.dropRight(1))(c.getClassLoader)
  }

  implicit def toReflectable(o: Any) = new {
    val c = o.getClass

    def getValue[T](name: String): T =
      c.getMethod(name).invoke(o).asInstanceOf[T]

    def setValue(name: String, value: Any): Unit = {
      val f = c.getDeclaredField(name)
      f.setAccessible(true)
      f.set(o, value)
    }

    def getFields[T](implicit m: Manifest[T]): Array[Field] = c.getDeclaredFields.filter {
      f => m.erasure.isAssignableFrom(f.getType)
    }
  }

  def getGenericType(field: Field): Class[_] = getGenericTypes(field).head
  def getGenericTypes(field: Field): List[Class[_]] =
    field.getGenericType.asInstanceOf[ParameterizedType]
    .getActualTypeArguments.toList.map(_.asInstanceOf[Class[_]])

  def isSeq(clazz: Class[_]): Boolean = classOf[Seq[_]].isAssignableFrom(clazz)
  def isOption(clazz: Class[_]): Boolean = clazz == classOf[Option[_]]
}

object ReflectionUtil extends ReflectionUtil
