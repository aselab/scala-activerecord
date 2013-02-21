package com.github.aselab.activerecord.inner

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import com.github.aselab.activerecord.aliases._
import com.github.aselab.activerecord.squeryl.Implicits._
import ActiveRecord._
import ReflectionUtil._

trait Relations {
  case class Parameters[T <: AR, JOINED_TYPE <: {def _1: T}, S](
    conditions: List[JOINED_TYPE => LogicalBoolean] = Nil,
    orders: List[JOINED_TYPE => ExpressionNode] = Nil,
    selector: JOINED_TYPE => S = {t: JOINED_TYPE => t._1},
    includeAssociations: List[T => Association[T, AR]] = Nil,
    pages: Option[(Int, Int)] = None,
    isUnique: Boolean = false
  )

  trait Relation[T <: AR, S] {
    type JOINED_TYPE <: {def _1: T}
    val parameters: Parameters[T, JOINED_TYPE, S]
    val queryable: Queryable[T]
    val manifest: Manifest[T]

    def conditions = parameters.conditions
    def orders = parameters.orders
    def includeAssociations = parameters.includeAssociations
    def pages = parameters.pages
    def isUnique = parameters.isUnique
    def selector = parameters.selector

    lazy val companion: ActiveRecordBaseCompanion[_, T] =
      classToCompanion(manifest.erasure).asInstanceOf[ActiveRecordBaseCompanion[_, T]]

    protected def copyParams[R](params: Parameters[T, JOINED_TYPE, R]): Relation[T, R]
    protected def copyParams[R](
      conditions: List[JOINED_TYPE => LogicalBoolean] = parameters.conditions,
      orders: List[JOINED_TYPE => ExpressionNode] = parameters.orders,
      selector: JOINED_TYPE => R = parameters.selector,
      includeAssociations: List[T => Association[T, AR]] = parameters.includeAssociations,
      pages: Option[(Int, Int)] = parameters.pages,
      isUnique: Boolean = parameters.isUnique
    ): Relation[T, R] = copyParams(Parameters(conditions, orders, selector, includeAssociations, pages, isUnique))

    private var _isLoaded = false
    def isLoaded: Boolean = _isLoaded

    private var _cache: List[S] = Nil
    private[inner] def cache = _cache
    private[inner] def cache_=(value: List[S]) = {
      _cache = value
      _isLoaded = true
      value
    }

    protected var sampleRecord: T = _

    def reload(implicit m: Manifest[S]): List[S] = inTransaction {
      cache = queryToIterable(toQuery).toList

      if (manifest == m && cache.nonEmpty) {
        val sources = cache.asInstanceOf[List[T]]
        val eagerLoadedMaps = includeAssociations.map(a =>
          (a, a(sampleRecord).eagerLoad(sources)(manifest))
        )
        for ((associationOf, map) <- eagerLoadedMaps; m <- sources) {
          associationOf(m).relation.cache = map.getOrElse(m.id, Nil)
        }
      }
      cache
    }

    def load(implicit m: Manifest[S]): List[S] = if (isLoaded) cache else reload

    def includes[A <: AR](associations: (T => Association[T, A])*): this.type =
      copyParams(includeAssociations = includeAssociations ++
        associations.toList.asInstanceOf[List[T => Association[T, AR]]])
        .asInstanceOf[this.type]

    protected def whereState(m: JOINED_TYPE) =
      dsl.where(LogicalBoolean.and(conditions.map(_.apply(m))))

    protected def ordersExpression(m: JOINED_TYPE) = orders.map(_.apply(m))

    protected def paginate[R](query: Query[R]) = pages.map {
      case (offset, count) => query.page(offset, count)
    }.getOrElse(query)

    protected def wrap[A <: {def _1: T}, R](f: T => R): A => R = {m: A => f(m._1)}
    protected def wrapTuple1[A <: {def _1: T}, R](f: Tuple1[T] => R): A => R =
      {t: A => f(Tuple1(t._1))}

    def head: S = try {
      headOption.get
    } catch { case e: java.util.NoSuchElementException =>
      throw ActiveRecordException.recordNotFound
    }

    def headOption: Option[S] = if (isLoaded) {
      cache.headOption
    } else {
      inTransaction { limit(1).toQuery.headOption }
    }

    def where(condition: T => LogicalBoolean): this.type =
      copyParams(conditions = conditions :+ wrap(condition)).asInstanceOf[this.type]

    def orderBy(conditions: (T => ExpressionNode)*): this.type =
      copyParams(orders = orders ++ conditions.map(wrap).toList).asInstanceOf[this.type]

    def select[R](selector: T => R): Relation[T, R] =
      copyParams(selector = wrap(selector))

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
      findAllBy(condition, conditions:_*).headOption
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
      findAllBy(name, value).headOption
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
      where(m => field.toEqualityExpression(m.getValue[Any](name), value))
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

    def distinct: this.type =
      copyParams(isUnique = true).asInstanceOf[this.type]

    /**
     * returns page results.
     * {{{
     * Post.all.orderBy(p => p.updatedAt desc).page(10 * (pageNumber - 1), 10)
     * }}}
     * @param offset offset count
     * @param count max count
     */
    def page(offset: Int, count: Int): this.type =
      copyParams(pages = Some(offset, count)).asInstanceOf[this.type]

    def exists(condition: T => LogicalBoolean): Boolean = inTransaction {
      where(condition).limit(1).count != 0
    }

    def count: Long = if (isUnique) toQuery.Count else nonNestQueryCount

    protected def nonNestQueryCount: Long

    def deleteAll()(implicit ev: S =:= T): List[T] = inTransaction {
      val records = toQuery.toList
      records.foreach(_.delete)
      records.asInstanceOf[List[T]]
    }

    def toQuery: Query[S]

    def toSql: String = inTransaction { toQuery.statement }
  }

  case class Relation1[T <: AR, S](
    parameters: Parameters[T, Tuple1[T], S],
    queryable: Queryable[T]
  )(implicit val manifest: Manifest[T]) extends Relation[T, S] {
    type JOINED_TYPE = Tuple1[T]

    protected def copyParams[R](params: Parameters[T, JOINED_TYPE, R]) =
      Relation1(params, queryable)

    def nonNestQueryCount: Long = paginate(
      from(queryable)(m => whereState(Tuple1(m)).compute(dsl.count))
    )

    def toQuery: Query[S] = paginate{
      val query = from(queryable){m =>
        sampleRecord = m
        val t = Tuple1(m)
        if (conditions.isEmpty) {
          dsl.select(selector(t)).orderBy(orders.map(_.apply(t)))
        } else {
          whereState(t).select(selector(t)).orderBy(orders.map(_.apply(t)))
        }
      }
      if (isUnique) query.distinct else query
    }

    def joins[J <: AR](on: (T, J) => LogicalBoolean)
      (implicit m: Manifest[J]): Relation2[T, J, S] = {
      val c = classToCompanion(m.erasure)
        .asInstanceOf[ActiveRecordBaseCompanion[_, J]]

      new Relation2(
        Parameters[T, (T, J), S](conditions.map(wrapTuple1), orders.map(wrapTuple1),
          wrapTuple1[(T, J), S](selector), includeAssociations, pages, isUnique),
        queryable, c.table, Function.tupled(on)
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
        Parameters[T, (T, J1, J2), S](conditions.map(wrapTuple1), orders.map(wrapTuple1),
          wrapTuple1[(T, J1, J2), S](selector), includeAssociations, pages, isUnique),
        queryable, c1.table, c2.table, Function.tupled(on)
      )(manifest)
    }
  }

  case class Relation2[T <: AR, J1 <: AR, S](
    parameters: Parameters[T, (T, J1), S],
    queryable: Queryable[T],
    joinTable: Queryable[J1],
    on: ((T, J1)) => LogicalBoolean
  )(implicit val manifest: Manifest[T]) extends Relation[T, S] {
    type JOINED_TYPE = (T, J1)

    protected def copyParams[R](params: Parameters[T, JOINED_TYPE, R]) =
      Relation2(params, queryable, joinTable, on)

    def where(condition: (T, J1) => LogicalBoolean): this.type =
      copyParams(conditions = conditions :+ Function.tupled(condition)).asInstanceOf[this.type]

    def select[R](selector: (T, J1) => R): Relation[T, R] =
      copyParams(selector = Function.tupled(selector))

    def orderBy(conditions: ((T, J1) => ExpressionNode)*): this.type =
      copyParams(orders = orders ++ conditions.toList.map(Function.tupled(_))).asInstanceOf[this.type]

    def nonNestQueryCount: Long = paginate(join(queryable, joinTable) {(m, j1) =>
      val t = (m, j1)  
      whereState(t).compute(dsl.count).on(on(t))
    })

    def toQuery: Query[S] = paginate{
      val query = join(queryable, joinTable) {(m, j1) =>
        val t = (m, j1)
        if (conditions.isEmpty) {
          dsl.select(selector(t)).orderBy(orders.map(_.apply(t))).on(on(t))
        } else {
          whereState(t).select(selector(t)).orderBy(orders.map(_.apply(t))).on(on(t))
        }
      }
      if (isUnique) query.distinct else query
    }
  }

  case class Relation3[T <: AR, J1 <: AR, J2 <: AR, S](
    parameters: Parameters[T, (T, J1, J2), S],
    queryable: Queryable[T],
    joinTable1: Queryable[J1],
    joinTable2: Queryable[J2],
    on: ((T, J1, J2)) => (LogicalBoolean, LogicalBoolean)
  )(implicit val manifest: Manifest[T]) extends Relation[T, S] {
    type JOINED_TYPE = (T, J1, J2)

    protected def copyParams[R](params: Parameters[T, JOINED_TYPE, R]) =
      Relation3(params, queryable, joinTable1, joinTable2, on)

    def where(condition: (T, J1, J2) => LogicalBoolean): this.type =
      copyParams(conditions = conditions :+ Function.tupled(condition)).asInstanceOf[this.type]

    def select[R](selector: (T, J1, J2) => R): Relation[T, R] =
      copyParams(selector = Function.tupled(selector))

    def orderBy(conditions: ((T, J1, J2) => ExpressionNode)*): this.type =
      copyParams(orders = orders ++ conditions.toList.map(Function.tupled(_))).asInstanceOf[this.type]

    def nonNestQueryCount: Long = paginate(join(queryable, joinTable1, joinTable2) {
      (m, j1, j2) =>
        val t = (m, j1, j2)
        val (on1, on2) = on(t)
        whereState(t).compute(dsl.count).on(on1, on2)
    })

    def toQuery: Query[S] = paginate{
      val query = join(queryable, joinTable1, joinTable2) {(m, j1, j2) =>
        val t = (m, j1, j2)
        val (on1, on2) = on(t)
        if (conditions.isEmpty) {
          dsl.select(selector(t)).orderBy(orders.map(_.apply(t))).on(on1, on2)
        } else {
          whereState(t).select(selector(t)).orderBy(orders.map(_.apply(t))).on(on1, on2)
        }
      }
      if (isUnique) query.distinct else query
    }
  }
}
