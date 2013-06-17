package com.github.aselab.activerecord.reflections

import com.github.aselab.activerecord._
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

  lazy val allFields: List[Field] = {
    def getFields(c: Class[_]): List[Field] = {
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
    val dateHandler = () => new java.util.Date()
    val timestampHandler = () => new java.sql.Timestamp(System.currentTimeMillis)
    val uuidHandler = () => java.util.UUID.randomUUID()
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
          case e: Throwable => throw ActiveRecordException.cannotCreateInstance(
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
  import ReflectionUtil._

  private lazy val annotationMap = annotations.map {
    a => (a.annotationType, a)
  }.toMap[Class[_], Annotation]

  lazy val isRequired = hasAnnotation[Required]
  lazy val isIgnored = hasAnnotation[Ignore]
  lazy val isUnique = hasAnnotation[Unique]
  lazy val isModel = ReflectionUtil.isModel(fieldType)

  def getAnnotation[T](implicit m: Manifest[T]): T =
    annotationMap(m.erasure).asInstanceOf[T]

  def hasAnnotation[T](implicit m: Manifest[T]): Boolean =
    annotationMap.isDefinedAt(m.erasure)

  def is[T](implicit m: Manifest[T]): Boolean = fieldType == m.erasure

  def setValue(model: Any, value: Any) {
    model.setValue(name, if (isOption) value.toOption else value)
  }
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
      case c if ClassInfo.factories.isDefinedAt(c) || isModel(c) =>
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
  private def _defaultClassLoader = Thread.currentThread.getContextClassLoader
  def defaultLoader: ClassLoader = Config.classLoader.getOrElse(_defaultClassLoader)

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
   * returns companion object from class
   * @param c class
   */
  def classToARCompanion[T <: ActiveRecordBase[_]](c: Class[_]): ActiveRecordBaseCompanion[_, T] =
    classToCompanion(c.getName)(c.getClassLoader)
      .asInstanceOf[ActiveRecordBaseCompanion[_, T]]

  /**
   * returns corresponding class from companion object
   * @param o companion object
   */
  def companionToClass(o: Any): Class[_] = {
    val c = o.getClass
    loadClass(c.getName.dropRight(1))(c.getClassLoader)
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
      def getField(c: Class[_], n: String): Field = try {
        c.getDeclaredField(n)
      } catch {
        case e: NoSuchFieldException =>
          Option(c.getSuperclass).map(getField(_, n)).getOrElse(throw e)
      }

      val f = getField(o.getClass, name)
      f.setAccessible(true)
      f.set(o, value)
    }

    def getFields[T](implicit m: Manifest[T]): Array[Field] =
      o.getClass.getDeclaredFields.filter {
        f => m.erasure.isAssignableFrom(f.getType)
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

  def getGenericType(field: Field): Class[_] = getGenericTypes(field).head
  def getGenericTypes(field: Field): List[Class[_]] =
    field.getGenericType.asInstanceOf[ParameterizedType]
    .getActualTypeArguments.toList.map(_.asInstanceOf[Class[_]])

  def isSeq(clazz: Class[_]): Boolean = classOf[Seq[_]].isAssignableFrom(clazz)
  def isOption(clazz: Class[_]): Boolean = clazz == classOf[Option[_]]
  def isModel(clazz: Class[_]): Boolean = classOf[inner.ProductModel].isAssignableFrom(clazz)
}

object ReflectionUtil extends ReflectionUtil
