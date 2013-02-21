package com.github.aselab.activerecord.inner

import com.github.aselab.activerecord._
import com.github.aselab.activerecord.dsl._
import com.github.aselab.activerecord.aliases._
import com.github.aselab.activerecord.squeryl.Implicits._
import ActiveRecord._
import reflections._
import ReflectionUtil._

trait Relations {
  case class Parameters[T <: AR, JoinedType <: {def _1: T}, S](
    conditions: List[JoinedType => LogicalBoolean] = Nil,
    orders: List[JoinedType => ExpressionNode] = Nil,
    selector: JoinedType => S = {t: JoinedType => t._1},
    includeAssociations: List[T => Association[T, AR]] = Nil,
    pages: Option[(Int, Int)] = None,
    isUnique: Boolean = false
  )

  trait QuerySupport[T <: AR, S] {
    type TupleN = {def _1: T}
    type JoinedType <: TupleN
    type Inc[A <: AR] = T => Association[T, A]

    val parameters: Parameters[T, JoinedType, S]

    // scalastyle:off
    def conditions = parameters.conditions
    def orders = parameters.orders
    def includeAssociations = parameters.includeAssociations
    def pages = parameters.pages
    def isUnique = parameters.isUnique
    def selector = parameters.selector
    // scalastyle:on

    protected def copyParams[R](params: Parameters[T, JoinedType, R]): Relation[T, R]

    protected def copyParams[R](
      conditions: List[JoinedType => LogicalBoolean] = parameters.conditions,
      orders: List[JoinedType => ExpressionNode] = parameters.orders,
      selector: JoinedType => R = parameters.selector,
      includeAssociations: List[T => Association[T, AR]] = parameters.includeAssociations,
      pages: Option[(Int, Int)] = parameters.pages,
      isUnique: Boolean = parameters.isUnique
    ): Relation[T, R] = copyParams(
      Parameters(conditions, orders, selector, includeAssociations, pages, isUnique)
    )

    protected implicit def relationToThisType[R](self: Relation[T, R]): this.type =
      self.asInstanceOf[this.type]

    private def _includes(associations: Inc[_]*): this.type =
      copyParams(includeAssociations =
        includeAssociations ++ associations.map(_.asInstanceOf[Inc[AR]]))

    def includes[A <: AR](association: Inc[A]): this.type =
      _includes(association)

    def includes[A1 <: AR, A2 <: AR](a1: Inc[A1], a2: Inc[A2]): this.type =
      _includes(a1, a2)

    def includes[A1 <: AR, A2 <: AR, A3 <: AR](
      a1: Inc[A1], a2: Inc[A2], a3: Inc[A3]
    ): this.type = _includes(a1, a2, a3)

    def includes[A1 <: AR, A2 <: AR, A3 <: AR, A4 <: AR](
      a1: Inc[A1], a2: Inc[A2], a3: Inc[A3], a4: Inc[A4]
    ): this.type = _includes(a1, a2, a3, a4)

    protected def whereState(m: JoinedType) =
      dsl.where(LogicalBoolean.and(conditions.map(_.apply(m))))

    protected def ordersExpression(m: JoinedType) = orders.map(_.apply(m))

    protected def paginate[R](query: Query[R]) = {
      val q = if (isUnique) query.distinct else query
      pages.map {
        case (offset, count) => q.page(offset, count)
      }.getOrElse(q)
    }

    protected def wrap[A <: TupleN, R](f: T => R): A => R = {m: A => f(m._1)}

    protected def wrapTuple1[A <: TupleN, R](f: Tuple1[T] => R): A => R =
      {t: A => f(Tuple1(t._1))}

    def where(condition: T => LogicalBoolean): this.type =
      copyParams(conditions = conditions :+ wrap(condition))

    def orderBy(conditions: (T => ExpressionNode)*): this.type =
      copyParams(orders = orders ++ conditions.map(wrap))

    def select[R](selector: T => R): Relation[T, R] =
      copyParams(selector = wrap(selector))

    /**
     * returns limited results.
     * {{{
     * Post.all.orderBy(p => p.updatedAt desc).limit(10)
     * }}}
     * @param count max count
     */
    def limit(count: Int): this.type =
      page(pages.map(_._1).getOrElse(0), count)

    def distinct: this.type =
      copyParams(isUnique = true)

    /**
     * returns page results.
     * {{{
     * Post.all.orderBy(p => p.updatedAt desc).page(10 * (pageNumber - 1), 10)
     * }}}
     * @param offset offset count
     * @param count max count
     */
    def page(offset: Int, count: Int): this.type =
      copyParams(pages = Some(offset, count))
  }

  trait Relation[T <: AR, S] extends QuerySupport[T, S] {
    val queryable: Queryable[T]
    val manifest: Manifest[T]

    lazy val companion: ActiveRecordBaseCompanion[_, T] =
      classToARCompanion(manifest.erasure)

    private var _isLoaded = false
    def isLoaded: Boolean = _isLoaded

    private var _cache: List[S] = Nil
    private[inner] def cache = _cache
    private[inner] def cache_=(value: List[S]) = {
      _cache = value
      _isLoaded = true
      value
    }

    def reload(implicit m: Manifest[S]): List[S] = inTransaction {
      cache = queryToIterable(toQuery).toList

      if (manifest == m && cache.nonEmpty) {
        val sources = cache.asInstanceOf[List[T]]
        val sample = companion.newInstance
        val eagerLoadedMaps = includeAssociations.map(a =>
          (a, a(sample).eagerLoad(sources)(manifest))
        )
        for ((associationOf, map) <- eagerLoadedMaps; m <- sources) {
          associationOf(m).relation.cache = map.getOrElse(m.id, Nil)
        }
      }
      cache
    }

    def load(implicit m: Manifest[S]): List[S] = if (isLoaded) cache else reload

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
      }

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

    def deleteAll()(implicit ev: S =:= T): List[T] = inTransaction {
      val records = toQuery.toList
      records.foreach(_.delete)
      records.asInstanceOf[List[T]]
    }

    def exists(condition: T => LogicalBoolean): Boolean = inTransaction {
      where(condition).limit(1).count != 0
    }

    def count: Long = if (isUnique) toQuery.Count else nonNestQueryCount

    protected def nonNestQueryCount: Long

    def toQuery: Query[S]

    def toSql: String = inTransaction { toQuery.statement }
  }

  case class Relation1[T <: AR, S](
    parameters: Parameters[T, Tuple1[T], S],
    queryable: Queryable[T]
  )(implicit val manifest: Manifest[T]) extends Relation[T, S] {
    type JoinedType = Tuple1[T]

    protected def copyParams[R](params: Parameters[T, JoinedType, R]) =
      Relation1(params, queryable)

    def nonNestQueryCount: Long = paginate(
      from(queryable)(m => whereState(Tuple1(m)).compute(dsl.count))
    )

    def toQuery: Query[S] = paginate(
      from(queryable){ m =>
        val t = Tuple1(m)
        val scope = if (conditions.isEmpty) dsl else whereState(t)
        scope.select(selector(t)).orderBy(orders.map(_.apply(t)))
      }
    )

    def joins[J <: AR](on: (T, J) => LogicalBoolean)
      (implicit m: Manifest[J]): Relation2[T, J, S] = {
      val c = classToARCompanion[J](m.erasure)

      Relation2(
        Parameters[T, (T, J), S](conditions.map(wrapTuple1), orders.map(wrapTuple1),
          wrapTuple1[(T, J), S](selector), includeAssociations, pages, isUnique),
        queryable, c.table, on.tupled
      )(manifest)
    }

    def joins[J1 <: AR, J2 <: AR](
      on: (T, J1, J2) => (LogicalBoolean, LogicalBoolean)
    )(implicit m1: Manifest[J1], m2: Manifest[J2]): Relation3[T, J1, J2, S] = {
      val c1 = classToARCompanion[J1](m1.erasure)
      val c2 = classToARCompanion[J2](m2.erasure)

      Relation3(
        Parameters[T, (T, J1, J2), S](conditions.map(wrapTuple1), orders.map(wrapTuple1),
          wrapTuple1[(T, J1, J2), S](selector), includeAssociations, pages, isUnique),
        queryable, c1.table, c2.table, on.tupled
      )(manifest)
    }
  }

  case class Relation2[T <: AR, J1 <: AR, S](
    parameters: Parameters[T, (T, J1), S],
    queryable: Queryable[T],
    joinTable: Queryable[J1],
    on: ((T, J1)) => LogicalBoolean
  )(implicit val manifest: Manifest[T]) extends Relation[T, S] {
    type JoinedType = (T, J1)

    protected def copyParams[R](params: Parameters[T, JoinedType, R]) =
      Relation2(params, queryable, joinTable, on)

    def where(condition: (T, J1) => LogicalBoolean): this.type =
      copyParams(conditions = conditions :+ condition.tupled)

    def select[R](selector: (T, J1) => R): Relation[T, R] =
      copyParams(selector = selector.tupled)

    def orderBy(conditions: ((T, J1) => ExpressionNode)*): this.type =
      copyParams(orders = orders ++ conditions.toList.map(_.tupled))

    def nonNestQueryCount: Long = paginate(join(queryable, joinTable) {(m, j1) =>
      val t = (m, j1)
      whereState(t).compute(dsl.count).on(on(t))
    })

    def toQuery: Query[S] = paginate(
      join(queryable, joinTable) {(m, j1) =>
        val t = (m, j1)
        val scope = if (conditions.isEmpty) dsl else whereState(t)
        scope.select(selector(t)).orderBy(orders.map(_.apply(t))).on(on(t))
      }
    )
  }

  case class Relation3[T <: AR, J1 <: AR, J2 <: AR, S](
    parameters: Parameters[T, (T, J1, J2), S],
    queryable: Queryable[T],
    joinTable1: Queryable[J1],
    joinTable2: Queryable[J2],
    on: ((T, J1, J2)) => (LogicalBoolean, LogicalBoolean)
  )(implicit val manifest: Manifest[T]) extends Relation[T, S] {
    type JoinedType = (T, J1, J2)

    protected def copyParams[R](params: Parameters[T, JoinedType, R]) =
      Relation3(params, queryable, joinTable1, joinTable2, on)

    def where(condition: (T, J1, J2) => LogicalBoolean): this.type =
      copyParams(conditions = conditions :+ condition.tupled)

    def select[R](selector: (T, J1, J2) => R): Relation[T, R] =
      copyParams(selector = selector.tupled)

    def orderBy(conditions: ((T, J1, J2) => ExpressionNode)*): this.type =
      copyParams(orders = orders ++ conditions.toList.map(_.tupled))

    def nonNestQueryCount: Long = paginate(join(queryable, joinTable1, joinTable2) {
      (m, j1, j2) =>
        val t = (m, j1, j2)
        val (on1, on2) = on(t)
        whereState(t).compute(dsl.count).on(on1, on2)
    })

    def toQuery: Query[S] = paginate(
      join(queryable, joinTable1, joinTable2) {(m, j1, j2) =>
        val t = (m, j1, j2)
        val (on1, on2) = on(t)
        val scope = if (conditions.isEmpty) dsl else whereState(t)
        scope.select(selector(t)).orderBy(orders.map(_.apply(t))).on(on1, on2)
      }
    )
  }
}
