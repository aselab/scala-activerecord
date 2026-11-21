package com.github.aselab.activerecord.reflections

import com.github.aselab.activerecord._
import org.joda.time.{LocalDate, DateTime}
import java.lang.annotation.Annotation
import java.lang.reflect.{Field, ParameterizedType}
import scala.util.control.Exception._
import scala.util.control.NonFatal
import annotations._
import scala.reflect.Selectable.reflectiveSelectable
import scala.reflect.ClassTag
import scala.language.implicitConversions

class ClassInfo[T <: AnyRef](val clazz: Class[T]) {
  import ClassInfo._

  val name = clazz.getName

  lazy val factory: () => T = {
    if (ReflectionUtil.isSeq(clazz)) {
      factories.nilHandler
    } else {
      getFactory(clazz)
    }
  }.asInstanceOf[() => T]

  lazy val allFields: List[Field] = {
    def getFields(c: Class[?]): List[Field] = {
      c.getDeclaredFields.toList.filterNot {f =>
        f.isAnnotationPresent(classOf[annotations.Ignore]) ||
        f.getName.startsWith("_") ||
        f.getName.contains("$")
      } ++ Option(c.getSuperclass).map(getFields).getOrElse(Nil)
    }

    getFields(clazz)
  }

  lazy val fieldInfo: Map[String, FieldInfo] = allFields.flatMap { f =>
    try {
      Some(f.getName -> FieldInfo(f, this))
    } catch {
      case e: ActiveRecordException => None
    }
  }.toMap

  /** element classes of generic fields such as `Option[Int]` (see [[FieldTypes]]) */
  lazy val genericTypes: Map[String, Class[?]] = ClassInfo.genericTypes(clazz)
}

object ClassInfo {
  private val cache = collection.mutable.Map[Class[?], ClassInfo[?]]()

  def apply[T <: AnyRef](clazz: Class[T]): ClassInfo[T] = cache.getOrElseUpdate(
    clazz, new ClassInfo(clazz)).asInstanceOf[ClassInfo[T]]

  /**
   * Field types registered for `clazz` and its superclasses. Initializes the companion object
   * first if needed, since model companions register the field types of their model.
   */
  def genericTypes(clazz: Class[?]): Map[String, Class[?]] = {
    def registered(c: Class[?]): Option[Map[String, Class[?]]] =
      FieldTypes.lookup(c).orElse {
        allCatch.opt(ReflectionUtil.classToCompanion(c))
        FieldTypes.lookup(c)
      }

    Iterator.iterate[Class[?]](clazz)(_.getSuperclass).takeWhile(c => c != null && c != classOf[Object])
      .toList.reverse.flatMap(c => registered(c).getOrElse(Map.empty)).toMap
  }

  object factories extends PrimitiveHandler[() => AnyRef] {
    val stringHandler = () => ""
    val booleanHandler = () => java.lang.Boolean.FALSE
    val intHandler = () => java.lang.Integer.valueOf(0)
    val longHandler = () => java.lang.Long.valueOf(0)
    val floatHandler = () => java.lang.Float.valueOf(0)
    val doubleHandler = () => java.lang.Double.valueOf(0.0)
    val dateHandler = () => new java.util.Date()
    val timestampHandler = () => new java.sql.Timestamp(System.currentTimeMillis)
    val localdateHandler = () => LocalDate.fromDateFields(new java.util.Date())
    val datetimeHandler = () => new DateTime(new java.sql.Timestamp(System.currentTimeMillis))
    val uuidHandler = () => java.util.UUID.randomUUID()
    val bigDecimalHandler = () => BigDecimal(0)
    val nilHandler = () => Nil.asInstanceOf[AnyRef]

    registrations ++= Seq(
      (classOf[Option[?]], () => None),
      (classOf[Array[Byte]], () => Array.empty[Byte]),
      (classOf[Map[?, ?]], () => Map.empty[String, String])
    )
  }

  def getFactory(clazz: Class[?]): () => AnyRef = factories.getOrRegister(clazz, {
    clazz.getConstructors.map(
      c => (c, c.getParameterTypes.toSeq)
    ).sortBy(_._2.size).to(LazyList).flatMap {
      case (const, params) => allCatch.opt {
        // test creation parameters
        val facts = params.map(c =>
          apply(c.asInstanceOf[Class[AnyRef]]).factory)
        facts.foreach(_.apply())

        () => try {
          const.newInstance(facts.map(_.apply())*).asInstanceOf[AnyRef]
        } catch {
          case NonFatal(e) => throw ActiveRecordException.cannotCreateInstance(
            clazz.getName, e.getMessage)
        }
      }
    }.headOption.getOrElse(throw ActiveRecordException.cannotCreateInstance(
      clazz.getName, "No usable constructor is found. It is recommended to implement default constructor.")
    )
  })
}

case class FieldInfo(
  name: String, fieldType: Class[?],
  isOption: Boolean, isSeq: Boolean,
  annotations: Seq[Annotation] = Nil
) {
  import ReflectionUtil._

  private lazy val annotationMap = annotations.map {
    a => (a.annotationType, a)
  }.toMap[Class[?], Annotation]

  lazy val isRequired = hasAnnotation[Required]
  lazy val isIgnored = hasAnnotation[Ignore]
  lazy val isUnique = hasAnnotation[Unique]
  lazy val isModel = ReflectionUtil.isModel(fieldType)

  def getAnnotation[T](implicit m: ClassTag[T]): T =
    annotationMap(m.runtimeClass).asInstanceOf[T]

  def hasAnnotation[T](implicit m: ClassTag[T]): Boolean =
    annotationMap.isDefinedAt(m.runtimeClass)

  def is[T](implicit m: ClassTag[T]): Boolean = fieldType == m.runtimeClass

  def setValue(model: Any, value: Any): Unit = {
    model.setValue(name, if (isOption) value.toOption else value)
  }

  def toSeq[T](model: Any): Seq[T] = if (isSeq) {
    model.getValue[Seq[T]](name)
  } else if (isOption) {
    model.getValue[Option[T]](name).toSeq
  } else {
    Option(model.getValue[T](name)).toSeq
  }
}

object FieldInfo {
  import ReflectionUtil._

  def apply(field: Field, classInfo: ClassInfo[?]): FieldInfo = {
    def notSupported: Nothing = throw ActiveRecordException.unsupportedType(
      classInfo.name + "#" + field.getName
    )

    def genericType: Class[?] = getGenericType(field) match {
      case c if c == classOf[Object] =>
        // the JVM erased a primitive type argument; use the types captured at compile time
        classInfo.genericTypes.getOrElse(field.getName, notSupported)

      case c => c
    }

    def fieldInfo(clazz: Class[?]): FieldInfo = clazz match {
      case c if isOption(c) =>
        fieldInfo(genericType).copy(isOption = true)
      case c if isSeq(c) =>
        fieldInfo(genericType).copy(isSeq = true)
      case c if ClassInfo.factories.isDefinedAt(c) || isModel(c) =>
        FieldInfo(field.getName, c, false, false, field.getAnnotations.toSeq)
      case c => notSupported
    }

    fieldInfo(field.getType)
  }
}

trait ReflectionUtil {
  private def _defaultClassLoader = Thread.currentThread.getContextClassLoader
  def defaultLoader: ClassLoader = Config.classLoader.getOrElse(_defaultClassLoader)
  /**
   * All superclasses and interfaces of `c`, excluding `c` itself:
   * the superclass chain first, then interfaces breadth-first, without duplicates.
   */
  def supertypes(c: Class[?]): Seq[Class[?]] = {
    val superclasses = Iterator.iterate[Class[?]](c.getSuperclass)(_.getSuperclass).takeWhile(_ != null).toList
    val interfaces = collection.mutable.LinkedHashSet[Class[?]]()
    var queue = (c :: superclasses).flatMap(_.getInterfaces.toList)
    while (queue.nonEmpty) {
      val i = queue.head
      queue = queue.tail
      if (interfaces.add(i)) queue = queue ++ i.getInterfaces.toList
    }
    superclasses ++ interfaces
  }

  def loadClass(name: String)(
    implicit classLoader: ClassLoader = defaultLoader
  ): Class[?] = classLoader.loadClass(name)

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
  def classToCompanion(c: Class[?]): Any =
    classToCompanion(c.getName)(using c.getClassLoader)

  /**
   * returns companion object from class
   * @param c class
   */
  def classToARCompanion[T <: ActiveRecordBase[?]](c: Class[?]): ActiveRecordBaseCompanion[?, T] =
    classToCompanion(c.getName)(using c.getClassLoader)
      .asInstanceOf[ActiveRecordBaseCompanion[?, T]]

  /**
   * returns corresponding class from companion object
   * @param o companion object
   */
  def companionToClass(o: Any): Class[?] = {
    val c = o.getClass
    loadClass(c.getName.dropRight(1))(using c.getClassLoader)
  }

  class Reflectable(o: Any) {
    def getValue[T](name: String): T = try {
      o.getClass.getMethod(name).invoke(o).asInstanceOf[T]
    } catch {
      case e: Exception =>
        val f = o.getClass.getDeclaredField(name)
        f.setAccessible(true)
        f.get(o).asInstanceOf[T]
    }

    def setValue(name: String, value: Any): Unit = {
      def getField(c: Class[?], n: String): Field = try {
        c.getDeclaredField(n)
      } catch {
        case e: NoSuchFieldException =>
          Option(c.getSuperclass).map(getField(_, n)).getOrElse(throw e)
      }

      val f = getField(o.getClass, name)
      f.setAccessible(true)
      f.set(o, value)
    }

    def getFields[T](implicit m: ClassTag[T]): Array[Field] =
      o.getClass.getDeclaredFields.filter {
        f => m.runtimeClass.isAssignableFrom(f.getType)
      }

    private def convertToOption[T](v: Any) = v match {
      case null | None => None
      case Some(o) => Some(o.asInstanceOf[T])
      case o => Some(o.asInstanceOf[T])
    }

    def toOption[T]: Option[T] = convertToOption[T](o)

    def getOption[T](name: String): Option[T] =
      convertToOption[T](getValue[Any](name))
  }

  implicit def toReflectable(o: Any): Reflectable = new Reflectable(o)

  def getGenericType(field: Field): Class[?] = getGenericTypes(field).head
  def getGenericTypes(field: Field): List[Class[?]] = getGenericTypes(field.getGenericType)
  def getGenericTypes(genericType: java.lang.reflect.Type): List[Class[?]] =
    genericType.asInstanceOf[ParameterizedType]
    .getActualTypeArguments.toList.map {
      case t: ParameterizedType => t.getRawType.asInstanceOf[Class[?]]
      case _t => _t.asInstanceOf[Class[?]]
    }

  def isSeq(clazz: Class[?]): Boolean = classOf[Seq[?]].isAssignableFrom(clazz)
  def isOption(clazz: Class[?]): Boolean = clazz == classOf[Option[?]]
  def isModel(clazz: Class[?]): Boolean = classOf[inner.ProductModel].isAssignableFrom(clazz)
  def toOption(value: Any): Option[?] = value match {
    case null | None => None
    case Some(v) => Some(v)
    case v => Some(v)
  }

  def isPrimitiveNumeric(clazz: Class[?]): Boolean = Seq(
    classOf[java.lang.Integer], classOf[Int],
    classOf[java.lang.Long], classOf[Long],
    classOf[java.lang.Float], classOf[Float],
    classOf[java.lang.Double], classOf[Double]
  ).exists(_ == clazz)

  def isBoolean(clazz: Class[?]): Boolean = Seq(
    classOf[java.lang.Boolean],
    classOf[Boolean]
  ).exists(_ == clazz)

  def isMap(clazz: Class[?]): Boolean = classOf[Map[?, ?]] == clazz
}

object ReflectionUtil extends ReflectionUtil
