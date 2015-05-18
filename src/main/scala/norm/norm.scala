package norm

import java.util.Date
import anorm._
import play.api.Play
import play.api.Play.current
import play.api.db.DB
import play.api.libs.json._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._
import scala.language.implicitConversions
import org.joda.time.DateTime

private trait NormedParameterValue

private trait Wrapper[T] {
  def value: T
}

object QueryOperation extends Enumeration {
  type QueryOperation = Value
  val EQ, NEQ, IN, GT, GTE, LT, LTE, CONTAINS, STARTS_WITH, ENDS_WITH = Value
}

import QueryOperation._

case class QueryCondition(eitherCondition: Either[(String, QueryOperation, Any), String]) {
  val whereCondition = eitherCondition match {
    case Right(query)    => query
    case Left(condition) => buildQueryFrom(condition._1, condition._2, condition._3)
  }

  def buildQueryFrom(column: String, queryOperation: QueryOperation, queryValue: Any): String ={
    val operation: String = queryOperation match {
      case EQ => {
        queryValue match {
          case None  => "is"
          case null  => "is"
          case _     => "="
        }
      }
      case NEQ => {
        queryValue match {
          case None  => "is not"
          case null  => "is not"
          case _     => "<>"
        }
      }
      case IN          => "in"
      case GT          => ">"
      case GTE         => ">="
      case LT          => "<"
      case LTE         => "<="
      case CONTAINS    => "like"
      case STARTS_WITH => "like"
      case ENDS_WITH   => "like"
      case _           => throw new RuntimeException(s"Could not find query operation '${queryOperation}'")
    }

    def preparedValue: String = queryValue match {
      case s: String    => s"'${queryValue}'"
      case s: List[Any] => s.map {
        case v: String  => s"'${v}'"
        case v          => v
      }.mkString(",")
      case d: DateTime  => s"'${d.toString("yyyy-MM-dd HH:mm:ss")}'"
      case d: Date      => s"'${new DateTime(d).toString("yyyy-MM-dd HH:mm:ss")}'"
      case null         => "null"
      case None         => "null"
      case _            => queryValue.toString
    }

    val value = queryOperation match {
      case IN           => s"(${preparedValue})"
      case CONTAINS     => s"'%${queryValue}%'"
      case STARTS_WITH  => s"'${queryValue}%'"
      case ENDS_WITH    => s"'%${queryValue}'"
      case _            => preparedValue
    }
    s"(${column} ${operation} ${value})"
  }

  def or(q: QueryCondition): QueryCondition = {
    QueryCondition(s"(${this.whereCondition} or ${q.whereCondition})")
  }

  def and(q: QueryCondition): QueryCondition = {
    QueryCondition(s"(${this.whereCondition} and ${q.whereCondition})")
  }
}

object QueryCondition {
  def apply(condition: (String, QueryOperation, Any)) = new QueryCondition(Left(condition))
  def apply(query: String) = new QueryCondition(Right(query))
}

case class NormedParameter(name: String, value: Any) {
  lazy val tupled: (String, Any) = (name, value)
  lazy val toNamedParameter: NamedParameter = NormProcessor.toNamedParameter(name, value)
}

object NormedParameter {
  import scala.language.implicitConversions

  implicit def string[V](t: (String, V)): NormedParameter = NormedParameter(t._1, t._2)
  implicit def symbol[V](t: (scala.Symbol, V)): NormedParameter = NormedParameter(t._1.name, t._2)
}

/**
 * Created by ricardo on 4/18/14.
 * Made better by gcaliari some time after that
 */

/**
 * Utility Methods to retrieve class metadata
 */
private object NormProcessor {

  /**
   * Discover the class properties with its types
   * @tparam T
   * the class to inspect
   * @return
   * (PropertyName -> PropertyType)
   */
  def constructorProperties[T: TypeTag] = synchronized {
    val tpe = typeOf[T]
    val constructor = tpe.decl(termNames.CONSTRUCTOR).asMethod
    constructor.paramLists.flatten.map { sym =>
      sym.name.toString -> tpe.member(sym.name).asMethod.returnType
    }
  }

  /**
   * Find the class constructor
   * @tparam T
   * the class to inspect
   * @return
   * constructor of T
   */
  private def classConstructorFor[T: TypeTag] = {
    val tpe = typeOf[T]
    val mirror = runtimeMirror(Play.current.classloader)
    val classType = tpe.typeSymbol.asClass
    val cm = mirror.reflectClass(classType)
    val ctor = tpe.decl(termNames.CONSTRUCTOR).asMethod
    cm.reflectConstructor(ctor)
  }

  /**
   * List with values to be applied to the constructor
   * of T
   * @param row
   * the Anorm row
   * @param tableName
   * The table Name
   * @tparam T
   * The class to be applied
   * @return
   * The value list
   */
  private def propListFrom[T: TypeTag](row: Row, tableName: String) = {
    val properties = NormProcessor.constructorProperties[T]
    val values = ListBuffer[Any]()
    val rowValuesMap = row.asMap

    val normalizedRowValuesMap = scala.collection.mutable.LinkedHashMap[String, Any]()

    rowValuesMap.toIndexedSeq.foreach[Unit] { (entry) =>
      normalizedRowValuesMap += entry._1.toLowerCase -> rowValuesMap.get(entry._1).get
    }

    val prefix = tableName.toLowerCase
    properties.foreach { property =>
      normalizedRowValuesMap.get(s"${prefix}.${property._1}".toLowerCase) match {
        case Some(a: None.type)                                                 => values += None
        case Some(a: Option[Any]) if property._2 <:< typeOf[BigDecimal]         => values += BigDecimal(a.get.asInstanceOf[java.math.BigDecimal])
        case Some(a: Option[Any]) if property._2 <:< typeOf[JsValue]            => values += Json.parse(a.get.asInstanceOf[org.postgresql.util.PGobject].getValue)
        case Some(a: Option[Any]) if property._2 <:< typeOf[Option[BigDecimal]] => values += a.map( a => BigDecimal(a.asInstanceOf[java.math.BigDecimal]) )
        case Some(a: Option[Any]) if property._2 <:< typeOf[Option[JsValue]]    => values += a.map( a => Json.parse(a.asInstanceOf[org.postgresql.util.PGobject].getValue))
        case Some(a: Option[Any]) if property._2 <:< typeOf[Option[Any]]        => values += a
        case Some(a: Option[Any])                                               => values += a.get
        case Some(a: Any) if property._2 <:< typeOf[BigDecimal]                 => values += BigDecimal(a.asInstanceOf[java.math.BigDecimal])
        case Some(a: Any) if property._2 <:< typeOf[JsValue]                    => values += Json.parse(a.asInstanceOf[org.postgresql.util.PGobject].getValue)
        case Some(a: Any) if property._2 <:< typeOf[Option[Any]]                => values += Some(a)
        case Some(a: Any)                                                       => values += a
        case None                                                               => throw new RuntimeException(s"Unhandled type for attribute=${prefix}.${property._1}")
      }
    }
    values
  }

  /**
   * Retrieves a instance of T from database represented by Row
   * @param row
   * @param tableName
   * @tparam T
   * @return
   * The database as a model of T
   */
  def instance[T: TypeTag](row: Row, tableName: String) = {
    val ctorm = classConstructorFor[T]
    val seqValues = propListFrom[T](row, tableName).toSeq
    ctorm(seqValues: _*)
  }

  /**
   * Finds the table name of class T
   * @param tableName
   * @tparam T
   * @return
   *
   */
  def tableName[T: TypeTag](tableName: Option[String]) = {
    if (tableName.isEmpty) typeOf[T].typeSymbol.name.toString else tableName.get
  }

  /**
   * The property representing the database id
   * TODO: get the right property
   */
  val id = "id"
  val creationDate = "createdAt"
  val updatedDate = "updatedAt"

  def toNamedParameter(name: String, value: Any): NamedParameter = {
    value match {
      case vType: BigDecimal         => (name -> vType.bigDecimal)
      case vType: JsValue            => (name -> Json.stringify(vType))
      case vType: Date               => (name -> vType)
      case vType: String             => (name -> vType)
      case vType: Int                => (name -> vType)
      case vType: Long               => (name -> vType)
      case vType: Double             => (name -> vType)
      case vType: Boolean            => (name -> vType)
      case Some(vType: BigDecimal)   => (name -> Some(vType.bigDecimal))
      case Some(vType: JsValue)      => (name -> Some(Json.stringify(vType)))
      case Some(vType: Date)         => (name -> Some(vType))
      case Some(vType: String)       => (name -> Some(vType))
      case Some(vType: Int)          => (name -> Some(vType))
      case Some(vType: Long)         => (name -> Some(vType))
      case Some(vType: Double)       => (name -> Some(vType))
      case Some(vType: Boolean)      => (name -> Some(vType))
      case None                      => (name -> None)
      case _                         => (name -> s"$value")
    }
  }

  def toNamedParameterWithUpdatedAt(properties: Seq[NormedParameter]): Seq[NamedParameter] = {
    properties.map { prop =>
      if (prop.name == NormProcessor.updatedDate) NormedParameter.string(prop.name -> new Date())
      else prop
    } map { _.toNamedParameter }
  }

}

abstract class Norm[T: TypeTag](tableNameOpt: Option[String] = None) extends DefaultNormQueries[T](tableNameOpt) {
  val id: Option[Long]
  val rm = runtimeMirror(Play.current.classloader)
  val tpe = typeOf[T]
  val idTerm = tpe.decl(TermName(NormProcessor.id)).asTerm

  def create(): Option[Long] = {
    val onSeq: Seq[NamedParameter] = attributes.map { att => NormProcessor.toNamedParameter(att, getFieldValue(att))}
    DB.withConnection { implicit c =>
      SQL(createSql).on(onSeq: _*).executeInsert()
    }
  }

  def createAndFind(): Option[T] = {
    create() match {
      case Some(id: Long) => refresh(id)
      case _ => None
    }
  }

  /**
   * Updates a database entry
   *
   * Will not update creationDate unless attribute is passed
   * Will update updatedDate with current date
   *
   * @param properties attributes to update, default is empty. if empty, updates all fields
   *                   in the model
   * @return number of the affected rows
   */
  def update(properties: NormedParameter*): Int = {
    val idParam: NamedParameter = (NormProcessor.id -> idValue)
    val updateProperties: Seq[NamedParameter] = if (properties.isEmpty) allProperties else NormProcessor.toNamedParameterWithUpdatedAt(properties)
    val updateValues: Seq[NamedParameter] = updateProperties :+ idParam
    DB.withConnection { implicit c =>
      SQL(updateQuery(updateProperties)).on(updateValues: _*).executeUpdate()
    }
  }

  /**
   * Updates a database entry
   *
   * Will not update creationDate unless attribute is passed
   * Will update updatedDate with current date
   *
   * @param properties attributes to update, default is empty. if empty, updates all fields
   *                   in the model
   * @return Some(updated object) or None if it wasn't updated
   */
  def updateAndFind(properties: NormedParameter*): Option[T] = {
    update(properties: _*) match {
      case numRows: Int if (numRows > 0) => refresh(idValue)
      case _ => None
    }
  }

  def save() = {
    if (id.isEmpty) {
      this.createAndFind()
    } else {
      this.updateAndFind()
    }
  }

  def refresh(): Option[T] = {
    refresh(id.get)
  }

  def refresh(id: Long = idValue): Option[T] = DB.withConnection {
    implicit c =>
      val forSelect = s" $selectQuery where ${NormProcessor.id} = {${NormProcessor.id}}"
      val query = SQL(forSelect).on(s"${NormProcessor.id}" -> id)
      query().collect {
        case r: Row => NormProcessor.instance[T](r, tableName).asInstanceOf[T]
      }.headOption
  }

  def delete() = DB.withConnection {
    implicit c =>
      val deleteQuery = s" DELETE FROM $tableName where ${NormProcessor.id} = {${NormProcessor.id}}"
      val query = SQL(deleteQuery).on(s"${NormProcessor.id}" -> idValue)
      query.executeUpdate()
  }

  def idValue: Long = {
    id match {
      case Some(id: Long) => id
      case _ => throw new RuntimeException(s"Could not find field with name '${NormProcessor.id}'")
    }
  }

  private def getFieldValue(fieldName: String): Any = {
    rm.reflect(this).reflectField(tpe.decl(TermName(fieldName)).asTerm).get
  }

  private def allProperties(): Seq[NamedParameter] = {
    val propertiesToUpdate = NormProcessor.constructorProperties[T].map(_._1).toSet - NormProcessor.creationDate - NormProcessor.id
    propertiesToUpdate.map { prop =>
      if (prop == NormProcessor.updatedDate) NamedParameter(prop, new Date())
      else NormProcessor.toNamedParameter(prop, getFieldValue(prop))
    }.toSeq
  }

}

/**
 * class to be extended in the companion objects.
 * This class adds some common database methods such as create,
 * find, etc...
 *
 * @param tableNameOpt
 * the database table name - defaults is the pluralization of the class name
 * @tparam T
 * model class to be represented
 */
abstract class NormCompanion[T: TypeTag](tableNameOpt: Option[String] = None) extends DefaultNormQueries[T](tableNameOpt) {

  implicit def toNamedParameter[V](np: Seq[NormedParameter]): Seq[NamedParameter] = np.map {
    _.toNamedParameter
  }

  /**
   * Creates a new database entry
   * @param properties
   * map containing the values to be added to the new database entry
   * @return
   * the do for the new database entry
   */
  def create(properties: NormedParameter*): Option[Long] = {
    val propertiesNames = properties.map(_.name)
    val propertiesValuesRef = properties.map { a => s"{${a.name}}"}

    val creationBuilder = new StringBuilder(s"insert into ${tableName}")
    creationBuilder.append(s"(${propertiesNames.mkString(",")})")
    creationBuilder.append(" values ")
    creationBuilder.append(s"(${propertiesValuesRef.mkString(",")})")
    val forCreation = creationBuilder.toString

    DB.withConnection { implicit c =>
      SQL(forCreation).on(properties: _*).executeInsert()
    }
  }

  /**
   * Updates a database entry
   * @param id
   * the id for the entry to be updated
   * @param properties
   * map containing the values to be updated to the database entry
   * @return
   * the count of updated row(s)
   */
  def update(id: Long, properties: NormedParameter*): Int = {
    val idParam: NamedParameter = (NormProcessor.id -> id)
    val updateProperties: Seq[NamedParameter] = NormProcessor.toNamedParameterWithUpdatedAt(properties)
    val updateValues: Seq[NamedParameter] = updateProperties :+ idParam

    DB.withConnection { implicit c =>
      SQL(updateQuery(updateProperties)).on(updateValues: _*).executeUpdate()
    }
  }

  /**
   * Deletes a database entry
   * @param id
   * the id for the entry to be deleted
   * @return
   * the count of updated row(s)
   */
  def delete(id: Long): Int = DB.withConnection {
    implicit c =>
      val deleteQuery = s" DELETE FROM $tableName where ${NormProcessor.id} = {${NormProcessor.id}}"
      val query = SQL(deleteQuery).on(s"${NormProcessor.id}" -> id)
      query.executeUpdate()
  }

  def findAll(): List[T] = runQuery(SQL(selectQuery))

  /**
   * Finds a database entry having the provided properties values
   * @param properties
   * map of the property
   * @return a list with the matched entries
   */
  def findBy(properties: NormedParameter*): List[T] = {
    if (properties.nonEmpty) {
      val whereClause = properties.map {
        prop => {
          prop.value match {
            case None => s"${prop.name} is null"
            case null => s"${prop.name} is null"
            case _ => s"${prop.name} = {${prop.name}}"
          }
        }
      }.mkString(" AND ")

      val forSelect = s" $selectQuery where ${whereClause}"
      val query: SimpleSql[Row] = SQL(forSelect).on(properties: _*)
      runQuery(query)
    } else {
      runQuery(SQL(selectQuery))
    }
  }

  /**
   * Examples of usage:
   *
   * val query1 = Q("columnA", EQ, "aaa").or(Q("columnB", > 1))
   * findBy(query1, orderBy = "columnB desc", limit = 10)
   *
   * val query2 = Q("columnA", EQ, "aaa").and(Q("columnB", > 1)).or(Q("columnC", STARTSWITH, "abc"))
   * findBy(query2, orderBy = "columnC asc", limit = 10)
   */
  def findBy(q: QueryCondition, orderBy: String = null, limit: Int = 0): List[T] = {
    var forSelect = s" $selectQuery "
    if (q != null && q.whereCondition != "") {
      forSelect = s"${forSelect} where ${q.whereCondition}"
    }
    if (orderBy != null && orderBy != "") {
      forSelect = s"${forSelect} order by ${orderBy}"
    }
    if (limit > 0) {
      forSelect = s"${forSelect} limit ${limit}"
    }
    val query = SQL(forSelect)
    runQuery(query)
  }

  def findById(id: Long): Option[T] = findBy("id" -> id).headOption

  def find(id: Long) = findBy(NormProcessor.id -> id).head

  def findOption(id: Long) = findBy(NormProcessor.id -> id).headOption

  def first(column: String): Option[T] = findBy(QueryCondition(""), orderBy = s"${column} asc", limit = 1).headOption

  def last(column: String): Option[T] = findBy(QueryCondition(""), orderBy = s"${column} desc", limit = 1).headOption

  def searchWith(query: String, onParams: Seq[NormedParameter]) = {
    runQuery(SQL(query).on(onParams: _*))
  }

  def runQuery(query: SimpleSql[Row]): List[T] = DB.withConnection { implicit c =>
    query().collect {
      case r: Row => NormProcessor.instance[T](r, tableName).asInstanceOf[T]
    }.toList
  }

  /**
   * Perform a action for each entry
   * @param f
   * @return
   */
  def foreach(f: T => Unit) = DB.withConnection {
    implicit c =>
      val forSelect = s"select * from ${tableName}"
      SQL(forSelect).apply().foreach {
        case r: Row => f(NormProcessor.instance[T](r, tableName).asInstanceOf[T])
      }
  }


  def findByQueryCondition(q: QueryCondition, orderBy: String = null, limit: Int = 0, offset: Int = 0): List[T] = DB.withConnection { implicit c =>
    var forSelect = s" ${selectQuery}"
    if (q != null && q.whereCondition != "") {
      forSelect = s"${forSelect} where ${q.whereCondition}"
    }
    if (orderBy != null && orderBy != "") {
      forSelect = s"${forSelect} order by ${orderBy}"
    }
    if (limit > 0) {
      forSelect = s"${forSelect} limit ${limit}"
    }
    if (offset > 0) {
      forSelect = s"${forSelect} offset ${offset}"
    }
    forSelect = s"${forSelect};"
    try {
      val query = SQL(forSelect)
      runQuery(query)
    } catch {
      case e: Throwable => {
        throw e
      }
    }
  }

  def count(q: QueryCondition = null): Long = DB.withConnection { implicit c =>
    val baseQuery: String = s"select count(${NormProcessor.id}) as total from ${tableName} "
    val forSelect = {
      if (q != null && q.whereCondition != "") {
        s"${baseQuery} where ${q.whereCondition} ;"
      } else {
        baseQuery + ";"
      }
    }
    val query = SQL(forSelect)
    query().collect {
      case r: Row => r[Long]("total")
    }.head
  }
}

abstract class DefaultNormQueries[T: TypeTag](tableNameOpt: Option[String] = None) {

  val tableName = if (tableNameOpt.isDefined) tableNameOpt.get else typeOf[T].typeSymbol.name.toString
  val attributeWithTypes: List[(String, reflect.runtime.universe.Type)] = NormProcessor.constructorProperties[T].filter {
    att => NormProcessor.id != att._1
  }

  lazy val attributesToSqlQueryMapping: Map[String, String] = attributeWithTypes.map { att =>
    att._2 match {
      case attType if attType <:< typeOf[JsValue] => att._1 -> s"CAST({${att._1}} AS json)"
      case attType if attType <:< typeOf[Option[JsValue]] => att._1 -> s"CAST({${att._1}} AS json)"
      case _ => att._1 -> s"{${att._1}}"
    }
  }.toMap

  lazy val tableNameAliasSufix = "1"
  lazy val tableNameAlias = tableName + tableNameAliasSufix
  lazy val attributes: Seq[String] = attributesToSqlQueryMapping.keys.toSeq
  lazy val csvAttributes = attributes.mkString(", ")
  lazy val csvAttributesWithTableName = tableNameAlias + "." + attributes.mkString(s", ${tableNameAlias}.")
  lazy val csvCurlyAttributes = attributesToSqlQueryMapping.values.mkString(",")

  lazy val createSql = s"INSERT INTO ${tableName} (${csvAttributes}) VALUES (${csvCurlyAttributes})"
  /**
   * @deprecated  As of release 2.3.7.4, replaced by {@link #selectQuery}
   */
  @Deprecated
  lazy val selectSql = s"SELECT $csvAttributes, ${NormProcessor.id} FROM ${tableName} "
  lazy val selectQuery = s"SELECT $csvAttributesWithTableName, ${tableNameAlias}.${NormProcessor.id} FROM ${tableName} ${tableNameAlias}"
  lazy val selectDistinctQuery = s"SELECT DISTINCT $csvAttributesWithTableName, ${tableNameAlias}.${NormProcessor.id} FROM ${tableName} ${tableNameAlias}"

  def updateQuery(updateProperties: Seq[NamedParameter]): String = {
    val updateContent = updateProperties.map { prop => s"${prop.name}=${attributesToSqlQueryMapping.get(prop.name).get}"}

    val updateBuilder = new mutable.StringBuilder(s"update ${tableName}")
    updateBuilder.append(" set ")
    updateBuilder.append(updateContent.mkString(","))
    updateBuilder.append(s" where ${NormProcessor.id}={${NormProcessor.id}}")
    updateBuilder.mkString
  }
}
