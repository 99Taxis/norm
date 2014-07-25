package norm

import java.util.Date

import anorm._
import play.api.Play.current
import play.api.db.DB
import play.api.libs.json._

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._
import scala.language.implicitConversions

private trait NormedParameterValue
private trait Wrapper[T] { def value: T }

case class NormedParameter(name: String, value: Any, namedParameter: NamedParameter) {
  lazy val tupled: (String, Any) = (name, value)
  lazy val toNamedParameter: NamedParameter = NormProcessor.toNamedParameter(value, namedParameter)
}

object NormedParameter {
  import scala.language.implicitConversions

  /**
   * Conversion to use tuple, with first element being name
   * of parameter as string.
   *
   * {{{
   * val p: Parameter = ("name" -> 1l)
   * }}}
   */
  implicit def string[V](t: (String, V))(implicit c: V => ParameterValue): NormedParameter = NormedParameter(t._1, t._2, t._1 -> c(t._2))

  /**
   * Conversion to use tuple,
   * with first element being symbolic name or parameter.
   *
   * {{{
   * val p: Parameter = ('name -> 1l)
   * }}}
   */
  implicit def symbol[V](t: (scala.Symbol, V))(implicit c: V => ParameterValue): NormedParameter = NormedParameter(t._1.name, t._2, t._1 -> c(t._2))
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
   **/
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
    val mirror = runtimeMirror(Thread.currentThread().getContextClassLoader)
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
        case Some(a: Option[Any]) if property._2 <:< typeOf[BigDecimal] => values += BigDecimal(a.get.asInstanceOf[java.math.BigDecimal])
        case Some(a: Option[Any]) if property._2 <:< typeOf[Option[Any]] => values += a
        case Some(a: Option[Any]) => values += a.get
        case Some(a: Any) if property._2 <:< typeOf[JsValue] => values += Json.parse(a.asInstanceOf[org.postgresql.util.PGobject].getValue)
        case Some(a: Any) if property._2 <:< typeOf[Option[Any]] => values += Some(a)
        case Some(a: Any) => values += a
        case None => throw new RuntimeException
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
    toNamedParameter(value, (name -> s"$value"))
  }

  def toNamedParameter(value: Any, np: NamedParameter): NamedParameter = {
    value match {
      case vType: BigDecimal         => (np.name -> vType.bigDecimal)
      case vType: JsValue            => (np.name -> Json.stringify(vType))
      case vType: Date               => (np.name -> vType)
      case vType: String             => (np.name -> vType)
      case vType: Int                => (np.name -> vType)
      case vType: Long               => (np.name -> vType)
      case vType: Double             => (np.name -> vType)
      case vType: Boolean            => (np.name -> vType)
      case Some(vType: BigDecimal)   => (np.name -> Some(vType.bigDecimal))
      case Some(vType: JsValue)      => (np.name -> Some(Json.stringify(vType)))
      case Some(vType: Date)         => (np.name -> Some(vType))
      case Some(vType: String)       => (np.name -> Some(vType))
      case Some(vType: Int)          => (np.name -> Some(vType))
      case Some(vType: Long)         => (np.name -> Some(vType))
      case Some(vType: Double)       => (np.name -> Some(vType))
      case Some(vType: Boolean)      => (np.name -> Some(vType))
      case None                      => (np.name -> None)
      case _                         => np
    }
  }

}

class Norm[T: TypeTag](tableNameOpt: Option[String] = None) extends DefaultNormQueries[T] {

  val rm = runtimeMirror(Thread.currentThread().getContextClassLoader)
  val tpe = typeOf[T]
  val idTerm = tpe.decl(TermName(NormProcessor.id)).asTerm


  /**
   * Updates a database entry
   *
   * Will not update creationDate unless attribute is passed
   * Will update updatedDate with current date
   *
   * @param properties attributes to update, default is empty. if empty, updates all fields
   *                   in the model
   * @return (TODO) number of the affected rows
   */
  def update(properties: NormedParameter*) = {
    val idParam: NamedParameter = (NormProcessor.id -> idValue)
    val updateProperties: Seq[NamedParameter] = if (properties.isEmpty) allPropertiesToUpdate() else checkUpdateDate(properties)
    val updateContent = updateProperties.map { prop => s"${prop.name}={${prop.name}}"}
    val queryProperties: Seq[NamedParameter] = updateProperties :+ idParam

    val updateBuilder = new StringBuilder(s"update ${tableName}")
    updateBuilder.append(" set ")
    updateBuilder.append(updateContent.mkString(","))
    updateBuilder.append(s" where ${NormProcessor.id}={${NormProcessor.id}}")
    val forUpdate = updateBuilder.mkString
    DB.withConnection { implicit c =>
      SQL(forUpdate).on(queryProperties: _*).executeUpdate() match {
        case numRows: Int if (numRows > 0) => refresh(idValue)
        case _ => None
      }
    }
  }

  private def allPropertiesToUpdate(): Seq[NamedParameter] ={
    val propertiesToUpdate = NormProcessor.constructorProperties[T].map(_._1).toSet - NormProcessor.creationDate - NormProcessor.id
    propertiesToUpdate.map{ prop =>
      if (prop == NormProcessor.updatedDate) NamedParameter(prop, new Date())
      else NormProcessor.toNamedParameter(prop, getFieldValue(prop))
    }.toSeq
  }

  private def checkUpdateDate(properties: Seq[NormedParameter]): Seq[NamedParameter] ={
    properties.map { prop =>
      if (prop.name == NormProcessor.updatedDate) NormedParameter.string(prop.name -> new Date()).toNamedParameter
      else prop.toNamedParameter
    }
  }

  def save(): Option[T] = {
    val onSeq: Seq[NamedParameter] = attributes.map { att => NormProcessor.toNamedParameter(att, getFieldValue(att)) }
    DB.withConnection { implicit c =>
      SQL(createSql).on(onSeq: _*).executeInsert() match {
        case Some(id: Long) => refresh(id)
        case _ => None
      }
    }
  }

  private def getFieldValue(fieldName: String): Any = {
    rm.reflect(this).reflectField(tpe.decl(TermName(fieldName)).asTerm).get
  }

  def refresh(id: Long = idValue): Option[T] = DB.withConnection {
    implicit c =>
      val forSelect = s" $selectSql where ${NormProcessor.id} = {${NormProcessor.id}}"
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
    rm.reflect(this).reflectField(idTerm).get match {
      case Some(id: Long) => id
      case _ => throw new RuntimeException(s"Could not find field with name '${NormProcessor.id}'")
    }
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
abstract class NormCompanion[T: TypeTag](tableNameOpt: Option[String] = None) extends DefaultNormQueries[T] {

  implicit def toNamedParameter[V](np: Seq[NormedParameter]): Seq[NamedParameter] = np.map{ _.toNamedParameter}

  /**
   * Creates a new database entry
   * @param properties
   * map containing the values to be added to the new database entry
   * @return
   * the do for the new database entry
   */
  def create(properties: NormedParameter*): Option[Long] = {
    val propertiesNames = properties.map(_.name)
    val propertiesValuesRef = properties.map{ a => s"{${a.name}}"}

    val creationBuilder = new StringBuilder(s"insert into ${tableName}")
    creationBuilder.append(s"(${propertiesNames.mkString(",")})")
    creationBuilder.append(" values ")
    creationBuilder.append(s"(${propertiesValuesRef.mkString(",")})")
    val forCreation = creationBuilder.toString

    DB.withConnection { implicit c =>
      SQL(forCreation).on(properties: _*).executeInsert()
    }
  }

  def searchWith(query: String, onParams: Seq[NormedParameter]) = DB.withConnection { implicit c =>
    SQL(query).on(onParams: _*)().collect {
      case r: Row => NormProcessor.instance[T](r, tableName).asInstanceOf[T]
    }.toList
  }

  /**
   * Finds a database entry having the provided properties values
   * @param properties
   * map of the property
   * @return a list with the matched entries
   */
  def findBy(properties: NormedParameter*): List[T] = DB.withConnection {
    implicit c =>
      var query = SQL(selectSql)()
      if (properties.nonEmpty) {
        val whereClause = properties.map { prop => s"${prop.name} = {${prop.name}}"}.mkString(" AND ")
        val forSelect = s" $selectSql where ${whereClause}"

        query = SQL(forSelect).on(properties: _*)()
      }
      runQuery(query)
  }

  def runQuery(query: Stream[Row]): List[T] = {
    query.collect {
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

  def find(id: Long) = findBy(NormProcessor.id -> id).head

  def findOption(id: Long) = findBy(NormProcessor.id -> id).headOption


}

abstract class DefaultNormQueries[T: TypeTag](tableNameOpt: Option[String] = None) {

  val tableName = if (tableNameOpt.isDefined) tableNameOpt.get else typeOf[T].typeSymbol.name.toString
  val attributeWithTypes: List[(String, reflect.runtime.universe.Type)] = NormProcessor.constructorProperties[T].filter {
    att => NormProcessor.id != att._1
  }
  val attributes: Seq[String] = attributeWithTypes.map(_._1)

  lazy val csvAttributes = attributes.mkString(",")
  lazy val csvCurlyAttributes2 = attributes.map(a => s"{${a}}").mkString(",")
  lazy val csvCurlyAttributes = attributeWithTypes.map { att =>
    att._2 match {
      case attType if attType <:< typeOf[JsValue] => s"CAST({${att._1}} AS json)"
      case attType => s"{${att._1}}"
    }
  }.mkString(",")


  lazy val createSql = s"INSERT INTO ${tableName} (${csvAttributes}) VALUES (${csvCurlyAttributes})"
  lazy val selectSql = s"SELECT $csvAttributes, ${NormProcessor.id} FROM ${tableName} "

}