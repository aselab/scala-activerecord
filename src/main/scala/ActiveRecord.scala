package com.github.aselab.activerecord

import org.squeryl._
import org.squeryl.internals.DatabaseAdapter
import org.squeryl.adapters._
import org.squeryl.PrimitiveTypeMode._
import java.util.{Date, UUID}
import java.sql.{Timestamp, DriverManager, Connection}
import com.mchange.v2.c3p0.ComboPooledDataSource
import com.typesafe.config._

/**
 * ActiveRecord 基底クラス.
 */
abstract class ActiveRecordBase extends KeyedEntity[Long] with Product with CRUDable {
  var id: Long = 0L

  /** モデルに対応するコンパニオンオブジェクト */
  lazy val _companion = ReflectionUtil.classToCompanion(getClass)
    .asInstanceOf[ActiveRecordCompanion[this.type]]

  override def isNewInstance = id == 0

  override def equals(obj: Any): Boolean = obj match {
    case p: Product => productIterator.toList == p.productIterator.toList
    case ar: AnyRef => super.equals(obj)
    case _ => false
  }

  protected def doCreate = {
    _companion.create(this)
    true
  }

  protected def doUpdate = {
    _companion.update(this)
    true
  }

  protected def doDelete = _companion.delete(id)
}

/** ActiveRecord 抽象基底クラス. */
abstract class ActiveRecord extends ActiveRecordBase

/**
 * ActiveRecord コンパニオンオブジェクトの基底クラス.
 */
trait ActiveRecordCompanion[T <: ActiveRecordBase] extends ReflectionUtil {
  /** 自己参照 */
  protected def self: this.type = this

  /** コンパニオンオブジェクトが参照するテーブル定義 */
  lazy val schema = Config.schema

  /**
   * コンパニオンオブジェクトが管理するテーブル.
   */
  implicit lazy val table: Table[T] = {
    val name = getClass.getSimpleName.dropRight(1)
    val field = name.head.toLower + name.tail + "Table"
    schema.getValue[Table[T]](field)
  }

  /**
   * 検索条件をchainするための暗黙変換.
   */
  implicit def toRichQuery(query: Query[T]) = new {
    def where(condition: (T) => dsl.ast.LogicalBoolean): Query[T] = {
      self.where(condition)(query)
    }

    def findBy(condition: (String, Any), conditions: (String, Any)*): Query[T] = {
      self.findBy(condition, conditions:_*)(query)
    }

    def findBy(name: String, value: Any): Query[T] = {
      self.findBy(name, value)(query)
    }

    /**
     * ソート
     * {{{
     * Person.findBy("country", "Japan").orderBy(p => p.age asc)
     * Person.all.orderBy(p => p.age asc, p => p.name asc)
     * }}}
     * @param condition 並べ替え条件
     * @param conditions 複数キーによるソート時の並べ替え条件
     */
    def orderBy(condition: (T) => dsl.ast.OrderByArg, conditions: (T => dsl.ast.OrderByArg)*) = {
      conditions.toList match {
        case Nil => from(query)(m => select(m).orderBy(condition(m)))
        case List(f1) => from(query)(m => select(m).orderBy(condition(m), f1(m)))
        case List(f1, f2) => from(query)(m => select(m).orderBy(condition(m), f1(m), f2(m)))
        case List(f1, f2, f3) => from(query)(m => select(m).orderBy(condition(m), f1(m), f2(m), f3(m)))
        case List(f1, f2, f3, f4) => from(query)(m => select(m).orderBy(condition(m), f1(m), f2(m), f3(m), f4(m)))
        case List(f1, f2, f3, f4, f5) => from(query)(m => select(m).orderBy(condition(m), f1(m), f2(m), f3(m), f4(m), f5(m)))
        case List(f1, f2, f3, f4, f5, f6) => from(query)(m => select(m).orderBy(condition(m), f1(m), f2(m), f3(m), f4(m), f5(m), f6(m)))
      }
    }

    /**
     * 指定件数取得
     * {{{
     * Post.all.orderBy(p => p.updatedAt desc).limit(10)
     * }}}
     * @param count 件数
     */
    def limit(count: Int) = query.page(0, count)
  }

  /**
   * 全件検索
   */
  def all = from(table)(m => select(m))

  /**
   * id検索
   */
  def apply(id: Long) = find(id)

  /**
   * id検索
   */
  def find(id: Long) = inTransaction { table.lookup(id) }

  /**
   * where検索.
   * {{{
   * 例: findBy {m: T => m.name === "abc" and m.age > 20}
   * }}}
   *
   * @param condition where句に相当する関数
   * @param query 検索元のテーブルやクエリ．デフォルトではtableから検索する．
   */
  def where(condition: (T) => dsl.ast.LogicalBoolean)(implicit query: Queryable[T]): Query[T] = {
    from(query)(m => PrimitiveTypeMode.where(condition(m)) select(m))
  }

  /**
   * Key-Value検索.
   * 複数条件のAND検索が可能．
   * {{{
   * 例: findBy("name" -> "abc", "email" -> "abc@foo.bar")
   * }}}
   * @param condition 
   */
  def findBy(condition: (String, Any), conditions: (String, Any)*)(implicit query: Queryable[T]): Query[T] = {
    conditions.foldLeft(findBy(condition._1, condition._2)(query)) {
      case (subquery, cond) => findBy(cond._1, cond._2)(subquery)
    }
  }

  /**
   * Key-Value検索.
   * {{{
   * 例: findBy("name", "abc")
   * }}}
   * @param name モデルのフィールド名
   * @param value 検索する値
   * @param query 検索元のテーブルやクエリ．デフォルトではtableから検索する．
   */
  def findBy(name: String, value: Any)(implicit query: Queryable[T]): Query[T] = {
    where(value match {
      case v: String => {m: T => m.getValue[String](name) === v}
      case Some(v: String) => {m: T => m.getValue[Option[String]](name) === Some(v)}
      case v: Boolean => {m: T => m.getValue[Boolean](name) === v}
      case Some(v: Boolean) => {m: T => m.getValue[Option[Boolean]](name) === Some(v)}
      case v: Int => {m: T => m.getValue[Int](name) === v}
      case Some(v: Int) => {m: T => m.getValue[Option[Int]](name) === Some(v)}
      case v: Long => {m: T => m.getValue[Long](name) === v}
      case Some(v: Long) => {m: T => m.getValue[Option[Long]](name) === Some(v)}
      case v: Float => {m: T => m.getValue[Float](name) === v}
      case Some(v: Float) => {m: T => m.getValue[Option[Float]](name) === Some(v)}
      case v: Double => {m: T => m.getValue[Double](name) === v}
      case Some(v: Double) => {m: T => m.getValue[Option[Double]](name) === Some(v)}
      case v: BigDecimal => {m: T => m.getValue[BigDecimal](name) === v}
      case Some(v: BigDecimal) => {m: T => m.getValue[Option[BigDecimal]](name) === Some(v)}
      case v: Timestamp => {m: T => m.getValue[Timestamp](name) === v}
      case Some(v: Timestamp) => {m: T => m.getValue[Option[Timestamp]](name) === Some(v)}
      case v: Date => {m: T => m.getValue[Date](name) === v}
      case Some(v: Date) => {m: T => m.getValue[Option[Date]](name) === Some(v)}
      case v: UUID => {m: T => m.getValue[UUID](name) === v}
      case Some(v: UUID) => {m: T => m.getValue[Option[UUID]](name) === Some(v)}
      case _ => throw new RuntimeException(
        "サポートしていない形式: %s by %s".format(name, value.toString))
    })(query)
  }

  /**
   * 対象テーブルのレコードを作成する.
   * @param model 作成するモデル
   */
  def create(model: T) = inTransaction {
    table.insert(model)
  }

  /**
   * 対象テーブルのレコードを更新する.
   * @param model 更新するモデル
   */
  def update(model: T) = inTransaction {
    table.update(model)
  }

  /**
   * 対象テーブルのレコードを削除する.
   * @param id 削除対象ID
   */
  def delete(id: Long) = inTransaction {
    table.delete(id)
  }

  /**
   * 対象テーブルのレコードを全削除する.
   */
  def deleteAll = inTransaction {
    table.delete(all)
  }

  /**
   * 重複チェック.
   */
  def isUnique(name: String, m: T): Boolean = inTransaction {
    val newValue = m.getValue[Any](name)
    // 値がnull または Noneの時は無条件でtrueを返す
    if (newValue == null || newValue == None) return true

    val result = findBy(name, newValue)
    find(m.id) match {
      // 既存レコードで値が変わっている時のみ重複チェック
      case Some(old) if old.getValue[Any](name) != newValue => result.isEmpty
      // 既存レコードで値が変わっていない場合は無条件でtrue
      case Some(_) => true
      // 新規レコードの場合
      case None => result.isEmpty
    }
  }

  /** Uniqueアノテーションが付与されたフィールド */
  lazy val uniqueFields =
    formatFields.filter(_.isAnnotationPresent(classOf[annotations.Unique]))

  /** 対象のcase class */
  private val targetClass = companionToClass(this)

  /**
   * インスタンス生成.
   * 対象クラスはデフォルトコンストラクタを実装する必要がある．
   */
  def newInstance = try {
    targetClass.newInstance.asInstanceOf[T]
  } catch {
    case e: InstantiationException =>
      throw ConventionException.defaultConstructorRequired
  }

  /** フィールド情報 */
  lazy val fieldInfo = {
    val m = newInstance
    formatFields.map { f =>
      val name = f.getName
      (name, FieldInfo(f, m.getValue[Any](name)))
    }.toMap
  }

  /** formatFieldのリストを返す */
  lazy val formatFields: List[java.lang.reflect.Field] =
    targetClass.getDeclaredFields.filterNot {f =>
      f.isAnnotationPresent(classOf[annotations.Ignore]) ||
      f.getName.contains("$")
    }.toList

  /** 値のコピーを行う */
  def merge(a: T, b: T): T = {
    fieldInfo.values.foreach {f =>
      val name = f.name
      a.setValue(name, b.getValue[Any](name))
    }
    a
  }

}

/**
 * スキーマ定義の基底クラス.
 */
trait ActiveRecordTables extends Schema {
  /** 全テーブル */
  def all: List[Table[_ <: ActiveRecordBase]]

  /** テーブルが生成済みかどうか */
  def isCreated = all.headOption.exists{ t => inTransaction {
    // 検索してエラーにならなければtrue
    try {
      t.lookup(1L)
      true
    } catch {
      case e => false
    }
  }}

  /** 初期化する */
  def initialize(implicit config: Map[String, Any] = Map()) {
    Config.conf = loadConfig(config)

    // 全テーブルのidフィールド定義
    all.foreach(on(_)(t => declare(
      t.id is(primaryKey, autoIncremented)
    )))

    SessionFactory.concreteFactory = Some(() => session)

    if (!isCreated) transaction { create }
  }

  def cleanup = Config.cleanup

  def loadConfig(config: Map[String, Any]): ActiveRecordConfig =
    DefaultConfig(config)

  def session = Session.create(Config.connection, Config.adapter)

  /** DBのdrop・createを行う */
  def reset = transaction {
    drop
    create
  }
}

trait ActiveRecordConfig {
  def schemaClass: String
  def connection: Connection
  def adapter: DatabaseAdapter
  def cleanup: Unit = {
    Session.cleanupResources
  }
}

case class DefaultConfig(map: Map[String, Any]) extends ActiveRecordConfig {
  val conf = ConfigFactory.load()
  val env = System.getProperty("run.mode", "dev")

  def get[T](key: String): Option[T] = map.get(key).map(_.asInstanceOf[T])
  def get[T](key: String, getter: String => T): Option[T] = try {
    Option(getter(env + "." + key))
  } catch {
    case e: ConfigException.Missing => None
  }
  def getString(key: String) = get[String](key).orElse(get(key, conf.getString))
  def getInt(key: String) = get[Int](key).orElse(get(key, conf.getInt))

  lazy val schemaClass = getString("schema").getOrElse("models.Tables")
  lazy val driverClass = getString("driver").getOrElse("org.h2.Driver")
  lazy val jdbcurl = getString("jdbcurl").getOrElse("jdbc:h2:mem:activerecord")
  lazy val username = getString("username")
  lazy val password = getString("password")
  lazy val minPoolSize = getInt("minpoolsize")
  lazy val maxPoolSize = getInt("maxpoolsize")

  lazy val adapter = driverClass match {
    case "org.h2.Driver" => new H2Adapter
    case "org.postgresql.Driver" => new PostgreSqlAdapter
    case "com.mysql.jdbc.Driver" => new MySQLAdapter
    case driver => throw new Exception("サポートしていないドライバ: " + driver)
  }

  /** データベースコネクションプール */
  lazy val pool = {
    System.setProperty("com.mchange.v2.log.MLog", "com.mchange.v2.log.FallbackMLog")
    System.setProperty("com.mchange.v2.log.FallbackMLog.DEFAULT_CUTOFF_LEVEL", "WARNING")

    val cpds = new ComboPooledDataSource
    cpds.setDriverClass(driverClass)
    cpds.setJdbcUrl(jdbcurl)
    username.foreach(cpds.setUser(_))
    password.foreach(cpds.setPassword(_))
    minPoolSize.foreach(cpds.setMinPoolSize(_))
    maxPoolSize.foreach(cpds.setMaxPoolSize(_))
    cpds
  }

  override def cleanup = {
    super.cleanup
    pool.close
  }

  def connection = pool.getConnection
}

/**
 * 設定管理オブジェクト
 */
object Config {
  var conf: ActiveRecordConfig = _

  lazy val schema = ReflectionUtil.classToCompanion(conf.schemaClass)
    .asInstanceOf[ActiveRecordTables]

  def connection = conf.connection
  def adapter = conf.adapter

  def cleanup = conf.cleanup
}

/**
 * マッピング時例外クラス.
 * @param msg メッセージ
 */
class ConventionException(msg: String) extends RuntimeException(msg)

/** マッピング時例外定義 */
object ConventionException {
  /** 未サポート例外 */
  def unsupportedType(name: String) = new ConventionException("サポートしていないタイプ: " + name)
  /** デフォルトコンストラクタ未実装例外 */
  def defaultConstructorRequired = new ConventionException("デフォルトコンストラクタを実装する必要があります")
  /** Option値型検出失敗例外 */
  def optionValueMustBeSome = new ConventionException("デフォルト値がNoneの場合type erasureによりGenericsの型パラメータを検出できません")
  /** リスト型検出失敗例外 */
  def traversableValueMustNotBeNil = new ConventionException("デフォルト値がNilの場合type erasureによりGenericsの型パラメータを検出できません")
  /** 型検出失敗例外 */
  def cannotDetectType(value: Any) = new ConventionException("%s の型を検出できません".format(value))
}

