package com.github.aselab.activerecord.inner

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import squeryl.Implicits._
import org.squeryl._
import org.squeryl.dsl._
import org.squeryl.dsl.ast._
import ReflectionUtil._

trait Relations {
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
