package norm

import java.util.Date

import play.api.libs.json._

import scala.reflect.runtime.universe._
import play.api.db.DB
import anorm._
import play.api.Play.current
import scala.collection.mutable.ListBuffer

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
    val constructor = tpe.declaration(nme.CONSTRUCTOR).asMethod
    constructor.paramss.reduceLeft(_ ++ _).map {
      sym => sym.name.toString -> tpe.member(sym.name).asMethod.returnType
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
    val m1 = runtimeMirror(Thread.currentThread().getContextClassLoader)
    val classType = tpe.typeSymbol.asClass
    val cm = m1.reflectClass(classType)
    val ctor = tpe.declaration(nme.CONSTRUCTOR).asMethod
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
  private def propListFrom[T: TypeTag](row: Row, tableName: Option[String]) = {
    val properties = NormProcessor.constructorProperties[T]
    val values = ListBuffer[Any]()
    val rowValuesMap = row.asMap

    val normalizedRowValuesMap = scala.collection.mutable.LinkedHashMap[String, Any]()

    rowValuesMap.toIndexedSeq.foreach[Unit] {
      (entry) =>
        normalizedRowValuesMap += entry._1.toLowerCase -> rowValuesMap.get(entry._1).get
    }

    val prefix = NormProcessor.tableName[T](tableName).toLowerCase
    properties.foreach {
      property =>
        normalizedRowValuesMap.get(s"${prefix}.${property._1}".toLowerCase) match {
          case Some(a: Option[Any]) if property._2 <:< typeOf[BigDecimal] => values += BigDecimal(a.get.asInstanceOf[java.math.BigDecimal])
          case Some(a: Option[Any]) if property._2 <:< typeOf[Option[Any]] => values += a
          case Some(a: Option[Any]) => values += a.get
          case Some(a: Any) if property._2 <:< typeOf[JsValue] => values += Json.parse(a.asInstanceOf[org.postgresql.util.PGobject].getValue)
          case Some(a: Any) if property._2 <:< typeOf[Pk[Any]] => values += Id(a)
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
  def instance[T: TypeTag](row: Row, tableName: Option[String]) = {
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
    if (tableName.isEmpty) typeOf[T].typeSymbol.name + "s" else tableName.get
  }

  /**
   * The property representing the database id
   * TODO: get the right property
   */
  val id = "id"
  val creationDate = "createdAt"
  val updatedDate = "updatedAt"

  def mapAttributeTypes(weirdAttributesMap: Map[String, ParameterValue[_]]): Seq[(String, ParameterValue[_])] = {
    weirdAttributesMap.map{ case (key, value) => (key, mapAttributeType(value)) }.toSeq
  }

  def mapAttributeType(attributeParameter: ParameterValue[_]): ParameterValue[_] = {
    attributeParameter.aValue match {
      case decimal: BigDecimal => anorm.toParameterValue(decimal.bigDecimal)
      case jsValue: JsValue => anorm.toParameterValue(Json.stringify(jsValue))
      case _ => attributeParameter
    }
  }
}


class Norm[T: TypeTag](tableNameOpt: Option[String] = None) extends DefaultNormQueries[T] {

  val rm = runtimeMirror(Thread.currentThread().getContextClassLoader)
  val tpe = typeOf[T]
  val idTerm = tpe.declaration(newTermName(NormProcessor.id)).asTerm



  /**
   * Updates a database entry
   *
   * Will not update creationDate unless attribute is passed
   * Will update updatedDate with current date
   *
   * @param attributes attributes to update, default is empty. if empty, updates all fields
   *                   in the model
   * @return (TODO) number of the affected rows
   */
  def update(attributes: Map[String, ParameterValue[_]] = Map()): Option[T] = {
    val providedProperties = if (attributes.isEmpty) NormProcessor.constructorProperties[T].map(_._1).toSet - NormProcessor.creationDate else attributes.keys.toSet
    val propertiesToUpdate = (providedProperties diff Set(NormProcessor.id)).toArray
    val defaultAttributes = scala.collection.mutable.Map[String, ParameterValue[_]]()

    val updateContent = ListBuffer[String]()
    propertiesToUpdate.foreach {
      prop =>
        updateContent += s"${prop}={${prop}}"
        defaultAttributes += prop -> (
          if (attributes.isEmpty)
            if(prop == NormProcessor.updatedDate) new Date() else getFieldValue(prop)
          else
            attributes.get(prop).get
        )
    }
    defaultAttributes += NormProcessor.id -> idValue

    val updateBuilder = new StringBuilder(s"update ${tableName}")
    updateBuilder.append(" set ")
    updateBuilder.append(updateContent.mkString(","))
    updateBuilder.append(s" where ${NormProcessor.id}={${NormProcessor.id}}")
    val forUpdate = updateBuilder.mkString
    DB.withConnection {
      implicit c =>
        SQL(forUpdate).on(NormProcessor.mapAttributeTypes(defaultAttributes.toMap): _*).executeUpdate() match {
          case numRows: Int if(numRows > 0) => Some(refresh(idValue))
          case _ => None
        }

    }
  }

  def save(): T = {
    val onMap = attributes.map { att => att -> NormProcessor.mapAttributeType(anorm.toParameterValue(getFieldValue(att))) }
    DB.withConnection {
      implicit c =>
        SQL(createSql).on(onMap.toSeq: _*).executeInsert() match {
          case (id: Any) => refresh(id)
        }
    }
  }

  private def getFieldValue(fieldName: String): Any = {
    rm.reflect(this).reflectField(tpe.declaration(newTermName(fieldName)).asTerm).get
  }

  def refresh(id: Any = idValue): T = DB.withConnection {
    implicit c =>
      val forSelect = s" $selectSql where ${NormProcessor.id} = {${NormProcessor.id}}"
      val query = SQL(forSelect).on(s"${NormProcessor.id}" -> id)
      query().collect {
        case r: Row => NormProcessor.instance[T](r, Some(tableName)).asInstanceOf[T]
      }.toList.head
  }

  def delete() = DB.withConnection {
    implicit c =>
      val deleteQuery = s" DELETE FROM $tableName where ${NormProcessor.id} = {${NormProcessor.id}}"
      val query = SQL(deleteQuery).on(s"${NormProcessor.id}" -> idValue)
      query.executeUpdate()
  }

  def idValue: Any = {
    rm.reflect(this).reflectField(idTerm).get
  }
}


/**
 * class to be extended in the companion objects.
 * This class adds some common database methods such as create,
 * find, etc...
 *
 * @tparam T
 * model class to be represented
 */
abstract class NormCompanion[T: TypeTag](tableNameOpt: Option[String] = None) extends DefaultNormQueries[T] {

  implicit val pkFormatter = new Format[Pk[Long]] {
    def reads(json: JsValue): JsResult[Pk[Long]] = JsSuccess(Id(json.as[Long]))
    def writes(id: Pk[Long]) = id match {
      case NotAssigned => Json.toJson(NotAssigned.toString)
      case Id(id: Long) => Json.toJson(id)
    }
  }

  /**
   * Creates a new database entry
   * @param attributes
   * map containing the values to be added to the new database entry
   * @return
   * the do for the new database entry
   */
  def create(attributes: Map[String, ParameterValue[_]]): Option[Long] = {
    val properties = (attributes.keySet diff Set(NormProcessor.id)).toArray
    val values = properties.seq.map(p => s"{${p}}")

    val creationBuilder = new StringBuilder(s"insert into ${NormProcessor.tableName[T](Some(tableName))}")
    creationBuilder.append(s"(${properties.mkString(",")})")
    creationBuilder.append(" values ")
    creationBuilder.append(s"(${values.mkString(",")})")
    val forCreation = creationBuilder.toString

    DB.withConnection {
      implicit c =>
        SQL(forCreation).on(attributes.toSeq: _*).executeInsert()
    }
  }

  /**
   * Finds a database entry having the provided property value
   * @param propertyName
   * the name of the property
   * @param propertyValue
   * the name of the property
   * @return a list with the matched entries
   */
  def findByProperty(propertyName: String, propertyValue: Any): List[T] = DB.withConnection {
    implicit c =>
      val forSelect = s" $selectSql where ${propertyName} = {${propertyName}}"
      val query = SQL(forSelect).on(s"$propertyName" -> propertyValue)
      runQuery(query())
  }

  /**
   * Finds a database entry having the provided properties values
   * @param propertyMap
   * map of the property
   * @return a list with the matched entries
   */
  def findByProperties(propertyMap: Map[String, Any]): List[T] = DB.withConnection {
    implicit c =>
      var query = SQL(selectSql)()
      if(propertyMap.nonEmpty){
        val whereClause = propertyMap.keys.map{ propName => s"${propName} = {${propName}}"}.mkString(" AND ")
        val forSelect = s" $selectSql where ${whereClause}"
        val onMap = NormProcessor.mapAttributeTypes(propertyMap.map { case (k, v) => k -> anorm.toParameterValue(v)})
        query = SQL(forSelect).on(onMap: _*)()
      }
      runQuery(query)
  }


  def runQuery(query: Stream[SqlRow]): List[T] = {
    val result = query.collect {
      case r: Row => NormProcessor.instance[T](r, Some(tableName)).asInstanceOf[T]
    }
    result.toList
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
        case r: Row => f(NormProcessor.instance[T](r, Some(tableName)).asInstanceOf[T])
      }
  }

  def find(id: Long) = findByProperty(NormProcessor.id, id).head

  def findOption(id: Long) = findByProperty(NormProcessor.id, id).headOption


}

abstract class DefaultNormQueries[T: TypeTag](tableNameOpt: Option[String] = None) {

  val tableName = if (tableNameOpt.isDefined) tableNameOpt.get else typeOf[T].typeSymbol.name.toString
  val attributeWithTypes: List[(String, reflect.runtime.universe.Type)] = NormProcessor.constructorProperties[T].filter {
    att => NormProcessor.id != att._1
  }
  val attributes: List[String] = attributeWithTypes.map(_._1)

  lazy val csvAttributes = attributes.mkString(",")
  lazy val csvCurlyAttributes2 = attributes.map(a => s"{${a}}").mkString(",")
  lazy val csvCurlyAttributes = attributeWithTypes.map{ att =>
    att._2 match {
      case attType if attType <:< typeOf[JsValue] => s"CAST({${att._1}} AS json)"
      case attType => s"{${att._1}}"
    }
  }.mkString(",")


  lazy val createSql = s"INSERT INTO ${tableName} (${csvAttributes}) VALUES (${csvCurlyAttributes})"
  lazy val selectSql = s"SELECT * FROM ${tableName} "

}