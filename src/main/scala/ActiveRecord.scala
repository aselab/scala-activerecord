package com.github.aselab.activerecord

import org.squeryl._
import org.squeryl.dsl._
import org.squeryl.dsl.ast._
import org.squeryl.PrimitiveTypeMode._
import squeryl.Implicits._

trait ActiveRecordBase[T] extends ProductModel with CRUDable
  with ActiveRecordBaseRelationSupport with ValidationSupport with IO
{
  def id: T
  def isPersisted: Boolean

  /** corresponding ActiveRecordCompanion object */
  lazy val recordCompanion = _companion.asInstanceOf[ActiveRecordBaseCompanion[T, this.type]]

  override def isNewRecord: Boolean = !isPersisted

  protected def doCreate = {
    recordCompanion.create(this)
    true
  }

  protected def doUpdate = {
    recordCompanion.update(this)
    true
  }

  protected def doDelete = recordCompanion.delete(id)

  protected lazy val relations: Map[(String, String), RelationWrapper[ActiveRecord, ActiveRecordBase[_]]] =
    recordCompanion.schema.relations
}

/**
 * Base class of ActiveRecord objects.
 *
 * This class provides object-relational mapping and CRUD logic and callback hooks.
 */
abstract class ActiveRecord extends ActiveRecordBase[Long]
  with ActiveRecordRelationSupport
{
  /** primary key */
  val id: Long = 0L

  def isPersisted: Boolean = id > 0
}

object ActiveRecord {
  case class Relation[T <: ActiveRecordBase[_]](
    conditions: Seq[T => LogicalBoolean] = Nil,
    orders: Seq[T => OrderByExpression] = Nil,
    pages: Option[(Int, Int)] = None
  )(implicit companion: ActiveRecordBaseCompanion[_, T], queryable: Queryable[T]) {

    private def whereState(m: T) =
      PrimitiveTypeMode.where(LogicalBoolean.and(conditions.map(_.apply(m))))

    private def ordersExpression(m: T) = orders.toList.map(_.apply(m))

    private def toQuery[R](selector: T => QueryYield[R]): Query[R] = {
      val query = from(queryable)(selector)
      pages.map {
        case (offset, count) => query.page(offset, count)
      }.getOrElse(query)
    }

    def where(condition: T => LogicalBoolean): Relation[T] =
      copy(conditions = conditions :+ condition)

    def findBy(condition: (String, Any), conditions: (String, Any)*): Option[T] =
      companion.findBy(condition, conditions:_*)(this)

    def findAllBy(condition: (String, Any), conditions: (String, Any)*): Relation[T] =
      companion.findAllBy(condition, conditions:_*)(this)

    def findBy(name: String, value: Any): Option[T] =
      companion.findBy(name, value)(this)

    def findAllBy(name: String, value: Any): Relation[T] =
      companion.findAllBy(name, value)(this)

    /**
     * sort results.
     *
     * {{{
     * Person.findAllBy("country", "Japan").orderBy(p => p.age asc)
     * Person.all.orderBy(p => p.age asc, p => p.name asc)
     * }}}
     * @param conditions sort conditions
     */
    def orderBy(conditions: (T => OrderByExpression)*): Relation[T] =
      copy(orders = orders ++ conditions.toList)

    /**
     * returns limited results.
     * {{{
     * Post.all.orderBy(p => p.updatedAt desc).limit(10)
     * }}}
     * @param count max count
     */
    def limit(count: Int): Relation[T] =
      copy(pages = pages.map(_._1 -> count).orElse(Some(0 -> count)))

    /**
     * returns page results.
     * {{{
     * Post.all.orderBy(p => p.updatedAt desc).page(10 * (pageNumber - 1), 10)
     * }}}
     * @param offset offset count
     * @param count max count
     */
    def page(offset: Int, count: Int): Relation[T] =
      copy(pages = Some((offset, count)))

    def count: Long = toQuery(m =>
      whereState(m).compute(PrimitiveTypeMode.count)
    )

    def toQuery: Query[T] = toQuery[T](m =>
      whereState(m).select(m).orderBy(ordersExpression(m))
    )

    def toSql: String = inTransaction { toQuery.statement }
  }
}

trait ActiveRecordBaseCompanion[K, T <: ActiveRecordBase[K]] extends ProductModelCompanion[T] with FormSupport[T] {
  import ReflectionUtil._
  import ActiveRecord._

  implicit val keyedEntityDef = new KeyedEntityDef[T, K] {
    def getId(m: T) = m.id
    def isPersisted(m: T) = m.isPersisted
    def idPropertyName = "id"
  }

  implicit def relationToIterable[A <% Relation[T]](relation: A): Iterable[T] =
    inTransaction { relation.toQuery.toList }

  /** self reference */
  protected def self: this.type = this

  /** database schema */
  lazy val schema = Config.schema

  /**
   * corresponding database table
   */
  implicit lazy val table: Table[T] = {
    val name = getClass.getName.dropRight(1)
    schema.tableMap(name).asInstanceOf[Table[T]]
  }

  /**
   * implicit conversion for query chain.
   */
  implicit def toRelation(query: Queryable[T]): Relation[T] =
    Relation[T]()(this, query)

  implicit def toRelation(r: ActiveRecordOneToMany[T]): Relation[T] =
    toRelation(r.relation)

  implicit def toRelation(t: this.type): Relation[T] = all

  implicit def toModel[A <: ActiveRecord](r: ActiveRecordManyToOne[A]): Option[A] = r.one

  implicit def toQueryable(t: this.type): Queryable[T] = t.table

  /**
   * all search.
   */
  implicit def all: Relation[T] = toRelation(table)

  /**
   * same as find method.
   */
  def apply(id: K): Option[T] = find(id)

  /**
   * search by id.
   */
  def find(id: K): Option[T] = inTransaction { table.lookup(id) }

  /**
   * query search.
   *
   * {{{
   * where {m: T => m.name === "abc" and m.age.~ > 20}
   * }}}
   *
   * @param condition search condition
   * @param query table or subquery in from clause. default is table
   */
  def where(condition: (T) => LogicalBoolean)(implicit relation: Relation[T]) =
    relation.where(condition)

  /**
   * Search by multiple fieldnames and values and return first record.
   *
   * {{{
   * findBy("name" -> "abc", "email" -> "abc@foo.bar")
   * }}}
   * @param condition fieldname-value tuple
   * @param conditions multiple fieldname-value tuples(optional)
   * @param query table or subquery in from clause. default is table
   */
  def findBy(condition: (String, Any), conditions: (String, Any)*)
    (implicit relation: Relation[T]): Option[T] = inTransaction {
    findAllBy(condition, conditions:_*)(relation).limit(1).headOption
  }

  /**
   * Search by multiple fieldnames and values.
   *
   * {{{
   * findAllBy("name" -> "abc", "email" -> "abc@foo.bar")
   * }}}
   * @param condition fieldname-value tuple
   * @param conditions multiple fieldname-value tuples(optional)
   * @param query table or subquery in from clause. default is table
   */
  def findAllBy(condition: (String, Any), conditions: (String, Any)*)
    (implicit relation: Relation[T]): Relation[T] = {
    conditions.foldLeft(findAllBy(condition._1, condition._2)(relation)) {
      case (r, cond) => findAllBy(cond._1, cond._2)(r)
    }
  }

  /**
   * Search by fieldname and value and return first record.
   * {{{
   * findBy("name", "abc")
   * }}}
   * @param name field name
   * @param value field value
   * @param query table or subquery in from clause. default is table
   */
  def findBy(name: String, value: Any)
    (implicit relation: Relation[T]): Option[T] = inTransaction {
    findAllBy(name, value)(relation).limit(1).headOption
  }

  /**
   * Search by fieldname and value.
   * {{{
   * findAllBy("name", "abc")
   * }}}
   * @param name field name
   * @param value field value
   * @param query table or subquery in from clause. default is table
   */
  def findAllBy(name: String, value: Any)(implicit relation: Relation[T]): Relation[T] = {
    val field = fieldInfo.getOrElse(name,
      throw ActiveRecordException.notFoundField(name)
    )

    val clause = {m: T =>
      val v1 = m.getValue[Any](name)
      val v2 = value
      field.toEqualityExpression(v1, v2)
    }
    where(clause)(relation)
  }

  /**
   * insert record from model.
   */
  protected[activerecord] def create(model: T) = inTransaction {
    table.insert(model)
  }

  /**
   * update record from model.
   */
  protected[activerecord] def update(model: T) = inTransaction {
    table.update(model)
  }

  /**
   * delete record from id.
   */
  protected[activerecord] def delete(id: K) = inTransaction {
    table.delete(id)
  }

  /**
   * delete all records.
   */
  def deleteAll(): List[T] = inTransaction {
    val models = all.toList
    models.foreach(_.delete)
    models
  }

  /**
   * unique validation.
   */
  def isUnique(name: String, m: T): Boolean = m.getValue[Any](name) match {
    case value if value == null || value == None =>
      true
    case value => inTransaction {
      find(m.id) match {
        case Some(old) if old.getValue[Any](name) != value =>
          findBy(name, value).isEmpty
        case Some(_) => true
        case None => findBy(name, value).isEmpty
      }
    }
  }

  /** Unique annotated fields */
  lazy val uniqueFields =
    formatFields.filter(_.isAnnotationPresent(classOf[annotations.Unique]))

  def fromMap(data: Map[String, Any]) {
    newInstance.assign(data)
  }
}

/**
 * Base class of ActiveRecord companion objects.
 *
 * This class provides database table mapping and query logic.
 */
trait ActiveRecordCompanion[T <: ActiveRecord] extends ActiveRecordBaseCompanion[Long, T] {
  import ActiveRecord._

  implicit def toRelationA[A <: ActiveRecordBase[_]](r: ActiveRecordManyToMany[T, A]): Relation[T] = toRelation(r.relation)

  implicit def toModelList(r: ActiveRecordOneToMany[T]): List[T] = r.toList
  implicit def toModelListA[A <: ActiveRecordBase[_]](r: ActiveRecordManyToMany[T, A]): List[T] = r.toList
}

