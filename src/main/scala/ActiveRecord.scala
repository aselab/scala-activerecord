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

  object Relation {
    def apply[K, T <: ActiveRecordBase[K], S](
      conditions: List[T => LogicalBoolean],
      orders: List[T => OrderByExpression],
      pages: Option[(Int, Int)],
      queryable: Queryable[T],
      companion: ActiveRecordBaseCompanion[K, T],
      selector: T => S
    ): Relation1[K, T, S] =
      Relation1(conditions, orders, pages, queryable, companion, selector)

    def apply[K, T <: ActiveRecordBase[K], S](
      queryable: Queryable[T],
      companion: ActiveRecordBaseCompanion[K, T],
      selector: T => S
    ): Relation1[K, T, S] = apply(Nil, Nil, None, queryable, companion, selector)
  }

  trait Relation[K, T <: ActiveRecordBase[K], S] {
    type JOINED_TYPE
    val conditions: List[JOINED_TYPE => LogicalBoolean]
    val orders: List[JOINED_TYPE => OrderByExpression]
    val pages: Option[(Int, Int)]
    val queryable: Queryable[T]
    val companion: ActiveRecordBaseCompanion[K, T]
    val selector: JOINED_TYPE => S

    protected def whereState(m: JOINED_TYPE) =
      PrimitiveTypeMode.where(LogicalBoolean.and(conditions.map(_.apply(m))))

    protected def ordersExpression(m: JOINED_TYPE) = orders.map(_.apply(m))

    protected def paginate[R](query: Query[R]) = pages.map {
      case (offset, count) => query.page(offset, count)
    }.getOrElse(query)

    protected def wrap[A <: {def _1: T}, R](f: T => R): A => R = {m: A => f(m._1)}

    def where(condition: T => LogicalBoolean): this.type

    /**
     * Search by multiple fieldnames and values and return first record.
     *
     * {{{
     * findBy("name" -> "abc", "email" -> "abc@foo.bar")
     * }}}
     * @param condition fieldname-value tuple
     * @param conditions multiple fieldname-value tuples(optional)
     */
    def findBy(condition: (String, Any), conditions: (String, Any)*): Option[S] = inTransaction {
      findAllBy(condition, conditions:_*).limit(1).toQuery.headOption
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
    def findAllBy(condition: (String, Any), conditions: (String, Any)*): this.type =
      conditions.foldLeft(findAllBy(condition._1, condition._2)) {
        case (r, cond) => r.findAllBy(cond._1, cond._2)
      }.asInstanceOf[this.type]

    /**
     * Search by fieldname and value and return first record.
     * {{{
     * findBy("name", "abc")
     * }}}
     * @param name field name
     * @param value field value
     */
    def findBy(name: String, value: Any): Option[S] = inTransaction {
      findAllBy(name, value).limit(1).toQuery.headOption
    }

    /**
     * Search by fieldname and value.
     * {{{
     * findAllBy("name", "abc")
     * }}}
     * @param name field name
     * @param value field value
     */
    def findAllBy(name: String, value: Any): this.type = {
      val field = companion.fieldInfo.getOrElse(name,
        throw ActiveRecordException.notFoundField(name)
      )

      val clause = {m: T =>
        val v1 = m.getValue[Any](name)
        val v2 = value
        field.toEqualityExpression(v1, v2)
      }
      where(clause)
    }

    /**
     * returns limited results.
     * {{{
     * Post.all.orderBy(p => p.updatedAt desc).limit(10)
     * }}}
     * @param count max count
     */
    def limit(count: Int): this.type = {
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
    def page(offset: Int, count: Int): this.type

    def count: Long

    def toQuery: Query[S]

    def toSql: String = inTransaction { toQuery.statement }
  }

  case class Relation1[K, T <: ActiveRecordBase[K], S](
    conditions: List[T => LogicalBoolean],
    orders: List[T => OrderByExpression],
    pages: Option[(Int, Int)],
    queryable: Queryable[T],
    companion: ActiveRecordBaseCompanion[K, T],
    selector: T => S
  ) extends Relation[K, T, S] {
    type JOINED_TYPE = T

    def where(condition: T => LogicalBoolean): this.type = {
      copy(conditions = conditions :+ condition).asInstanceOf[this.type]
    }

    def select[R](selector: T => R): Relation[K, T, R] = {
      copy(selector = selector)
    }

    def orderBy(conditions: (T => OrderByExpression)*): this.type = {
      copy(orders = orders ++ conditions.toList).asInstanceOf[this.type]
    }

    def page(offset: Int, count: Int): this.type = {
      copy(pages = Some(offset, count)).asInstanceOf[this.type]
    }

    def count: Long = paginate(
      from(queryable)(m => whereState(m).compute(PrimitiveTypeMode.count))
    )

    def toQuery: Query[S] = paginate(
      from(queryable)(m =>
        if (conditions.isEmpty) {
          PrimitiveTypeMode.select(selector(m)).orderBy(ordersExpression(m))
        } else {
          whereState(m).select(selector(m)).orderBy(ordersExpression(m))
        }
      )
    )

    def joins[J <: ActiveRecordBase[_]](on: (T, J) => LogicalBoolean)
      (implicit m: Manifest[J]): Relation2[K, T, J, S] = {
      val c = classToCompanion(m.erasure)
        .asInstanceOf[ActiveRecordBaseCompanion[_, J]]

      new Relation2(
        conditions.map(wrap),
        orders.map(wrap), pages,
        queryable, (c.table, on),
        companion, wrap(selector)
      )
    }
  }

  case class Relation2[K, T <: ActiveRecordBase[K], J1 <: ActiveRecordBase[_], S](
    conditions: List[((T, J1)) => LogicalBoolean],
    orders: List[((T, J1)) => OrderByExpression],
    pages: Option[(Int, Int)],
    queryable: Queryable[T],
    join1: (Queryable[J1], (T, J1) => LogicalBoolean),
    companion: ActiveRecordBaseCompanion[K, T],
    selector: ((T, J1)) => S
  ) extends Relation[K, T, S] {
    type JOINED_TYPE = (T, J1)

    def where(condition: T => LogicalBoolean): this.type = {
      copy(conditions = conditions :+ wrap(condition)).asInstanceOf[this.type]
    }

    def where(condition: (T, J1) => LogicalBoolean): this.type =
      copy(conditions = conditions :+ Function.tupled(condition)).asInstanceOf[this.type]

    def select[R](selector: (T, J1) => R): Relation[K, T, R] =
      copy(selector = Function.tupled(selector))

    def orderBy(conditions: ((T, J1) => OrderByExpression)*): this.type =
      copy(orders = orders ++ conditions.toList.map(Function.tupled(_))).asInstanceOf[this.type]

    def page(offset: Int, count: Int): this.type =
      copy(pages = Some(offset, count)).asInstanceOf[this.type]

    def count: Long = paginate(join(queryable, join1._1)(
      (m, j1) => whereState((m, j1)).compute(PrimitiveTypeMode.count)
        .on(join1._2(m, j1))
    ))

    def toQuery: Query[S] = paginate(
      join(queryable, join1._1) {(m, j1) =>
        if (conditions.isEmpty) {
          PrimitiveTypeMode.select(selector((m, j1))).orderBy(ordersExpression((m, j1))).on(join1._2(m, j1))
        } else {
          whereState((m, j1)).select(selector((m, j1))).orderBy(ordersExpression((m, j1))).on(join1._2(m, j1))
        }
      }
    )
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

  implicit def relationToIterable[S](relation: Relation[K, T, S]): Iterable[S] =
    inTransaction { relation.toQuery.toList }

  implicit def relationableToIterable[A <% Relation[K, T, T]](relation: A): Iterable[T] =
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
  implicit def toRelation(query: Queryable[T]): Relation1[K, T, T] =
    Relation(query, this, identity)

  implicit def toRelation(r: ActiveRecordOneToMany[T]): Relation1[K, T, T] =
    toRelation(r.relation)

  implicit def toRelation(t: this.type): Relation1[K, T, T] =
    toRelation(t.table)

  implicit def toModel[A <: ActiveRecord](r: ActiveRecordManyToOne[A]): Option[A] = r.one

  /**
   * all search.
   */
  implicit def all: Relation1[K, T, T] = toRelation(table)

  /**
   * same as find method.
   */
  def apply(id: K): Option[T] = find(id)

  /**
   * search by id.
   */
  def find(id: K): Option[T] = inTransaction { table.lookup(id) }

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
          this.findBy(name, value).isEmpty
        case Some(_) => true
        case None => this.findBy(name, value).isEmpty
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

  implicit def toRelationA[A <: ActiveRecordBase[_]](r: ActiveRecordManyToMany[T, A]): Relation[Long, T, T] = toRelation(r.relation)

  implicit def toModelList(r: ActiveRecordOneToMany[T]): List[T] = r.toList
  implicit def toModelListA[A <: ActiveRecordBase[_]](r: ActiveRecordManyToMany[T, A]): List[T] = r.toList
}

