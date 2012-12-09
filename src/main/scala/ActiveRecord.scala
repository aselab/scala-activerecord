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
  import ReflectionUtil._

  class Relation[T <: ActiveRecordBase[_], S](
    val conditions: List[T => LogicalBoolean],
    val orders: List[T => OrderByExpression],
    val pages: Option[(Int, Int)],
    queryable: Queryable[T],
    companion: ActiveRecordBaseCompanion[_, T],
    selector: T => S
  ) {
    def this(
      queryable: Queryable[T],
      companion: ActiveRecordBaseCompanion[_, T],
      selector: T => S
    ) = this(Nil, Nil, None, queryable, companion, selector)

    private def whereState(m: T) =
      PrimitiveTypeMode.where(LogicalBoolean.and(conditions.map(_.apply(m))))

    private def ordersExpression(m: T) = orders.map(_.apply(m))

    private def toQuery[R](selector: T => QueryYield[R]): Query[R] = {
      val query = from(queryable)(selector)
      pages.map {
        case (offset, count) => query.page(offset, count)
      }.getOrElse(query)
    }

    def where(condition: T => LogicalBoolean): Relation[T, S] = {
      new Relation(conditions :+ condition, orders, pages,
        queryable, companion, selector)
    }

    def findBy(condition: (String, Any), conditions: (String, Any)*): Option[S] =
      companion.findBy(condition, conditions:_*)(this)

    def findAllBy(condition: (String, Any), conditions: (String, Any)*): Relation[T, S] =
      companion.findAllBy(condition, conditions:_*)(this)

    def findBy(name: String, value: Any): Option[S] =
      companion.findBy(name, value)(this)

    def findAllBy(name: String, value: Any): Relation[T, S] =
      companion.findAllBy(name, value)(this)

    def select[R](selector: T => R): Relation[T, R] = {
      new Relation[T, R](conditions, orders, pages, queryable, companion, selector)
    }

    /**
     * sort results.
     *
     * {{{
     * Person.findAllBy("country", "Japan").orderBy(p => p.age asc)
     * Person.all.orderBy(p => p.age asc, p => p.name asc)
     * }}}
     * @param conditions sort conditions
     */
    def orderBy(conditions: (T => OrderByExpression)*): Relation[T, S] = {
      new Relation(this.conditions, orders ++ conditions.toList, pages,
        queryable, companion, selector)
    }

    /**
     * returns limited results.
     * {{{
     * Post.all.orderBy(p => p.updatedAt desc).limit(10)
     * }}}
     * @param count max count
     */
    def limit(count: Int): Relation[T, S] = {
      page(pages.map(_._1).getOrElse(0) , count)
    }

    /**
     * returns page results.
     * {{{
     * Post.all.orderBy(p => p.updatedAt desc).page(10 * (pageNumber - 1), 10)
     * }}}
     * @param offset offset count
     * @param count max count
     */
    def page(offset: Int, count: Int): Relation[T, S] = {
      new Relation(conditions, orders, Some(offset, count),
        queryable, companion, selector)
    }

    def count: Long = toQuery(m =>
      whereState(m).compute(PrimitiveTypeMode.count)
    )

    def toQuery: Query[S] = toQuery[S] {m =>
      if (conditions.isEmpty) {
        PrimitiveTypeMode.select(selector(m)).orderBy(ordersExpression(m))
      } else {
        whereState(m).select(selector(m)).orderBy(ordersExpression(m))
      }
    }

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

  implicit def relationToIterable[S](relation: Relation[T, S]): Iterable[S] =
    inTransaction { relation.toQuery.toList }

  implicit def relationableToIterable[A <% Relation[T, T]](relation: A): Iterable[T] =
    relationToIterable(relation)

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
  implicit def toRelation(query: Queryable[T]): Relation[T, T] =
    new Relation(query, this, identity)

  implicit def toRelation(r: ActiveRecordOneToMany[T]): Relation[T, T] =
    toRelation(r.relation)

  implicit def toRelation(t: this.type): Relation[T, T] =
    toRelation(t.table)

  implicit def toModel[A <: ActiveRecord](r: ActiveRecordManyToOne[A]): Option[A] = r.one

  implicit def toQueryable(t: this.type): Queryable[T] = t.table

  /**
   * all search.
   */
  implicit def all: Relation[T, T] = toRelation(table)

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
   */
  def where[S](condition: (T) => LogicalBoolean)(implicit relation: Relation[T, S]) =
    relation.where(condition)

  /**
   * Search by multiple fieldnames and values and return first record.
   *
   * {{{
   * findBy("name" -> "abc", "email" -> "abc@foo.bar")
   * }}}
   * @param condition fieldname-value tuple
   * @param conditions multiple fieldname-value tuples(optional)
   */
  def findBy[S](condition: (String, Any), conditions: (String, Any)*)
    (implicit relation: Relation[T, S]): Option[S] = inTransaction {
    findAllBy(condition, conditions:_*)(relation).limit(1).toQuery.headOption
  }

  /**
   * Search by multiple fieldnames and values.
   *
   * {{{
   * findAllBy("name" -> "abc", "email" -> "abc@foo.bar")
   * }}}
   * @param condition fieldname-value tuple
   * @param conditions multiple fieldname-value tuples(optional)
   */
  def findAllBy[S](condition: (String, Any), conditions: (String, Any)*)
    (implicit relation: Relation[T, S]): Relation[T, S] = {
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
   */
  def findBy[S](name: String, value: Any)
    (implicit relation: Relation[T, S]): Option[S] = inTransaction {
    findAllBy(name, value)(relation).limit(1).toQuery.headOption
  }

  /**
   * Search by fieldname and value.
   * {{{
   * findAllBy("name", "abc")
   * }}}
   * @param name field name
   * @param value field value
   */
  def findAllBy[S](name: String, value: Any)
    (implicit relation: Relation[T, S]): Relation[T, S] = {
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

  implicit def toRelationA[A <: ActiveRecordBase[_]](r: ActiveRecordManyToMany[T, A]): Relation[T, T] = toRelation(r.relation)

  implicit def toModelList(r: ActiveRecordOneToMany[T]): List[T] = r.toList
  implicit def toModelListA[A <: ActiveRecordBase[_]](r: ActiveRecordManyToMany[T, A]): List[T] = r.toList
}

