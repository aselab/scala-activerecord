package com.github.aselab.activerecord.inner

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import com.github.aselab.activerecord.aliases._
import ActiveRecord._
import squeryl.Implicits._
import org.squeryl._
import org.squeryl.dsl._
import org.squeryl.dsl.ast._
import ReflectionUtil._

trait Relations {
  object Relation {
    def apply[T <: AR, S](
      conditions: List[T => LogicalBoolean],
      orders: List[T => OrderByExpression],
      pages: Option[(Int, Int)],
      queryable: Queryable[T],
      selector: T => S
    )(implicit m: Manifest[T]): Relation1[T, S] =
      Relation1(conditions, orders, pages, queryable, selector)(m)

    def apply[T <: AR, S](
      queryable: Queryable[T],
      selector: T => S
    )(implicit m: Manifest[T]): Relation1[T, S] =
      apply(Nil, Nil, None, queryable, selector)(m)
  }

  trait Relation[T <: AR, S] {
    type JOINED_TYPE
    val conditions: List[JOINED_TYPE => LogicalBoolean]
    val orders: List[JOINED_TYPE => ExpressionNode]
    val pages: Option[(Int, Int)]
    val queryable: Queryable[T]
    val selector: JOINED_TYPE => S
    val includeAssociations: List[T => Association[T, AR]]
    val manifest: Manifest[T]
    lazy val companion: ActiveRecordBaseCompanion[_, T] =
      classToCompanion(manifest.erasure).asInstanceOf[ActiveRecordBaseCompanion[_, T]]

    private var _isLoaded = false
    def isLoaded: Boolean = _isLoaded

    private var _cache: List[S] = Nil
    private[inner] def cache = _cache
    private[inner] def cache_=(value: List[S]) = {
      _cache = value
      _isLoaded = true
      value
    }

    private[inner] var eagerLoadingAssociations: List[Association[T, AR]] = Nil

    def reload(implicit m: Manifest[S]): List[S] = inTransaction {
      cache = queryToIterable(toQuery).toList

      if (manifest == m && !cache.isEmpty) {
        val records = cache.asInstanceOf[List[T]]
        val mapList = eagerLoadingAssociations.map(a => Association.eagerLoad(a, records)(manifest))
        for ((association, map) <- includeAssociations.zip(mapList); m <- records) {
          association(m).relation.cache = map.getOrElse(m.id, Nil)
        }
      }
      cache
    }

    def load(implicit m: Manifest[S]): List[S] = if (isLoaded) cache else reload

    def includes[A <: AR](associations: (T => Association[T, A])*): this.type

    protected def whereState(m: JOINED_TYPE) =
      PrimitiveTypeMode.where(LogicalBoolean.and(conditions.map(_.apply(m))))

    protected def ordersExpression(m: JOINED_TYPE) = orders.map(_.apply(m))

    protected def paginate[R](query: Query[R]) = pages.map {
      case (offset, count) => query.page(offset, count)
    }.getOrElse(query)

    protected def wrap[A <: {def _1: T}, R](f: T => R): A => R = {m: A => f(m._1)}

    def head = try {
      headOption.get
    } catch { case e: java.util.NoSuchElementException =>
      throw ActiveRecordException.recordNotFound
    }

    def headOption = if (isLoaded) {
      cache.headOption
    } else {
      inTransaction { limit(1).toQuery.headOption }
    }

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

    def findByOrCreate(m: T, field: String, fields: String*)(implicit ev: T =:= S): S = {
      findBy((field, m.getValue(field)),
        fields.map(f => (f, m.getValue(f))).toSeq:_*).getOrElse(m.create)
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

    def exists(condition: T => LogicalBoolean): Boolean = inTransaction {
      where(condition).limit(1).count != 0
    }

    def count: Long

    def toQuery: Query[S]

    def toSql: String = inTransaction { toQuery.statement }
  }

  case class Relation1[T <: AR, S](
    conditions: List[T => LogicalBoolean],
    orders: List[T => ExpressionNode],
    pages: Option[(Int, Int)],
    queryable: Queryable[T],
    selector: T => S,
    includeAssociations: List[T => Association[T, AR]] = Nil
  )(implicit val manifest: Manifest[T]) extends Relation[T, S] {
    type JOINED_TYPE = T

    def where(condition: T => LogicalBoolean): this.type = {
      copy(conditions = conditions :+ condition).asInstanceOf[this.type]
    }

    def select[R](selector: T => R): Relation[T, R] = {
      copy(selector = selector)
    }

    def orderBy(conditions: (T => ExpressionNode)*): this.type = {
      copy(orders = orders ++ conditions.toList).asInstanceOf[this.type]
    }

    def page(offset: Int, count: Int): this.type = {
      copy(pages = Some(offset, count)).asInstanceOf[this.type]
    }

    def count: Long = paginate(
      from(queryable)(m => whereState(m).compute(PrimitiveTypeMode.count))
    )

    def toQuery: Query[S] = paginate(
      from(queryable){m =>
        eagerLoadingAssociations = includeAssociations.map(_.apply(m))
        if (conditions.isEmpty) {
          PrimitiveTypeMode.select(selector(m)).orderBy(orders.map(_.apply(m)))
        } else {
          whereState(m).select(selector(m)).orderBy(orders.map(_.apply(m)))
        }
      }
    )

    def includes[A <: AR](associations: (T => Association[T, A])*): this.type = {
      copy(includeAssociations = includeAssociations ++ associations.toList.asInstanceOf[List[T => Association[T, AR]]])
        .asInstanceOf[this.type]
    }

    def joins[J <: AR](on: (T, J) => LogicalBoolean)
      (implicit m: Manifest[J]): Relation2[T, J, S] = {
      val c = classToCompanion(m.erasure)
        .asInstanceOf[ActiveRecordBaseCompanion[_, J]]

      new Relation2(
        conditions.map(wrap),
        orders.map(wrap), pages,
        queryable, c.table, Function.tupled(on),
        wrap[(T, J), S](selector)
      )(manifest)
    }

    def joins[J1 <: AR, J2 <: AR](
      on: (T, J1, J2) => (LogicalBoolean, LogicalBoolean)
    )(implicit m1: Manifest[J1], m2: Manifest[J2]): Relation3[T, J1, J2, S] = {
      val c1 = classToCompanion(m1.erasure)
        .asInstanceOf[ActiveRecordBaseCompanion[_, J1]]
      val c2 = classToCompanion(m2.erasure)
        .asInstanceOf[ActiveRecordBaseCompanion[_, J2]]

      new Relation3(
        conditions.map(wrap),
        orders.map(wrap), pages,
        queryable, c1.table, c2.table, Function.tupled(on),
        wrap[(T, J1, J2), S](selector)
      )(manifest)
    }
  }

  case class Relation2[T <: AR, J1 <: AR, S](
    conditions: List[((T, J1)) => LogicalBoolean],
    orders: List[((T, J1)) => ExpressionNode],
    pages: Option[(Int, Int)],
    queryable: Queryable[T],
    joinTable: Queryable[J1],
    on: ((T, J1)) => LogicalBoolean,
    selector: ((T, J1)) => S,
    includeAssociations: List[T => Association[T, AR]] = Nil
  )(implicit val manifest: Manifest[T]) extends Relation[T, S] {
    type JOINED_TYPE = (T, J1)

    def where(condition: T => LogicalBoolean): this.type = {
      copy(conditions = conditions :+ wrap(condition)).asInstanceOf[this.type]
    }

    def where(condition: (T, J1) => LogicalBoolean): this.type =
      copy(conditions = conditions :+ Function.tupled(condition)).asInstanceOf[this.type]

    def select[R](selector: (T, J1) => R): Relation[T, R] =
      copy(selector = Function.tupled(selector))

    def orderBy(conditions: ((T, J1) => ExpressionNode)*): this.type =
      copy(orders = orders ++ conditions.toList.map(Function.tupled(_))).asInstanceOf[this.type]

    def page(offset: Int, count: Int): this.type =
      copy(pages = Some(offset, count)).asInstanceOf[this.type]

    def count: Long = paginate(join(queryable, joinTable) {(m, j1) =>
      val t = (m, j1)  
      whereState(t).compute(PrimitiveTypeMode.count).on(on(t))
    })

    def includes[A <: AR](associations: (T => Association[T, A])*): this.type = {
      copy(includeAssociations = includeAssociations ++ associations.toList.asInstanceOf[List[T => Association[T, AR]]])
        .asInstanceOf[this.type]
    }

    def toQuery: Query[S] = paginate(
      join(queryable, joinTable) {(m, j1) =>
        val t = (m, j1)
        if (conditions.isEmpty) {
          PrimitiveTypeMode.select(selector(t)).orderBy(orders.map(_.apply(t))).on(on(t))
        } else {
          whereState(t).select(selector(t)).orderBy(orders.map(_.apply(t))).on(on(t))
        }
      }
    )
  }

  case class Relation3[T <: AR, J1 <: AR, J2 <: AR, S](
    conditions: List[((T, J1, J2)) => LogicalBoolean],
    orders: List[((T, J1, J2)) => ExpressionNode],
    pages: Option[(Int, Int)],
    queryable: Queryable[T],
    joinTable1: Queryable[J1],
    joinTable2: Queryable[J2],
    on: ((T, J1, J2)) => (LogicalBoolean, LogicalBoolean),
    selector: ((T, J1, J2)) => S,
    includeAssociations: List[T => Association[T, AR]] = Nil
  )(implicit val manifest: Manifest[T]) extends Relation[T, S] {
    type JOINED_TYPE = (T, J1, J2)

    def where(condition: T => LogicalBoolean): this.type = {
      copy(conditions = conditions :+ wrap(condition)).asInstanceOf[this.type]
    }

    def where(condition: (T, J1, J2) => LogicalBoolean): this.type =
      copy(conditions = conditions :+ Function.tupled(condition)).asInstanceOf[this.type]

    def select[R](selector: (T, J1, J2) => R): Relation[T, R] =
      copy(selector = Function.tupled(selector))

    def orderBy(conditions: ((T, J1, J2) => OrderByExpression)*): this.type =
      copy(orders = orders ++ conditions.toList.map(Function.tupled(_))).asInstanceOf[this.type]

    def page(offset: Int, count: Int): this.type =
      copy(pages = Some(offset, count)).asInstanceOf[this.type]

    def count: Long = paginate(join(queryable, joinTable1, joinTable2) {
      (m, j1, j2) =>
        val t = (m, j1, j2)
        val (on1, on2) = on(t)
        whereState(t).compute(PrimitiveTypeMode.count).on(on1, on2)
    })

    def includes[A <: AR](associations: (T => Association[T, A])*): this.type = {
      copy(includeAssociations = includeAssociations ++ associations.toList.asInstanceOf[List[T => Association[T, AR]]]).asInstanceOf[this.type]
    }

    def toQuery: Query[S] = paginate(
      join(queryable, joinTable1, joinTable2) {(m, j1, j2) =>
        val t = (m, j1, j2)
        val (on1, on2) = on(t)
        if (conditions.isEmpty) {
          PrimitiveTypeMode.select(selector(t)).orderBy(orders.map(_.apply(t))).on(on1, on2)
        } else {
          whereState(t).select(selector(t)).orderBy(orders.map(_.apply(t))).on(on1, on2)
        }
      }
    )
  }
}
