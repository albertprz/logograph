package orm

import java.sql.{Connection, ResultSet, PreparedStatement, Statement, Date, Time}
import java.math.BigDecimal
import scala.util.Try
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.{universe => ru}
import utils.ReflectionUtils

class ScalaQLContext (conn: Connection) {

  import ScalaQLContext._

  def run [T: ru.TypeTag] (query: SelectQuery[_, T]) =
    runQuery(query).get

  def tryRun [T: ru.TypeTag] (query: SelectQuery[_, T]) =
    runQuery(query)

  private def runQuery [T: ru.TypeTag] (query: SelectQuery[_, T]) = Try {

    val companion = ReflectionUtils.companionOf[T]
    val parameters = query.params.values.toList
    val sql = flattenQuery(query.sql, parameters)
    val params = flattenParameters(parameters)

    val stmt = conn.prepareStatement(sql)
    parameteriseStatement(stmt, params)

    val resultSet = stmt.executeQuery()
    val results = ListBuffer[T]()

    while (resultSet.next()) {
      val ctorArgs = getCtorArgs(resultSet, companion.paramTypes)
      results += companion.apply(ctorArgs).asInstanceOf[T]
    }

    results.toList
  }
}

object ScalaQLContext {

  private def flattenQuery (sql: String, params: List[Any]) =
    params.filter(_.isInstanceOf[List[Any]])
          .foldLeft(sql) {
            case (acc, curr: List[Any]) => acc.replaceFirst("in [?]", curr.map(x => "?")
                                                                          .mkString("in (", ", ", ")"))
        }

  private def flattenParameters (params: List[Any]) =
    params flatMap { _ match {
        case list: List[Any] => list
        case other: Any => List(other)
      }
    }

  private def parameteriseStatement (stmt: PreparedStatement, params: List[Any]) =
    for (i <- 0 to params.size - 1) {
      params(i) match {
        case int: Int        => stmt.setInt(i + 1, int)
        case long: Long      => stmt.setLong(i + 1, long)
        case float: Float    => stmt.setFloat(i + 1, float)
        case double: Double  => stmt.setDouble(i + 1, double)
        case bool: Boolean   => stmt.setBoolean(i + 1, bool)
        case str: String     => stmt.setString(i + 1, str)
        case dec: BigDecimal => stmt.setBigDecimal(i + 1, dec)
        case date: Date      => stmt.setDate(i + 1, date)
        case time: Time      => stmt.setTime(i + 1, time)
        case other: Any      => throw new Exception("Unknown types cannot be used in queries " +
                                                    "for constant or runtime parameter values: \n" +
                                                    s"""|Type: ${other.getClass.getSimpleName}
                                                        |Value: $other""".stripMargin)
      }
    }

  private def getCtorArgs (resultSet: ResultSet, paramTypes: Array[String]) =
    for (i <- 0 to paramTypes.size - 1)
        yield paramTypes(i) match {
          case "int"                  => resultSet.getInt(i + 1)
          case "long"                 => resultSet.getLong(i + 1)
          case "float"                => resultSet.getFloat(i + 1)
          case "double"               => resultSet.getDouble(i + 1)
          case "boolean"              => resultSet.getBoolean(i + 1)
          case "java.lang.String"     => resultSet.getString(i + 1)
          case "java.lang.BigDecimal" => resultSet.getBigDecimal(i + 1)
          case "java.sql.date"        => resultSet.getDate(i + 1)
          case "java.sql.time"        => resultSet.getTime(i + 1)
          case other @ _              => throw new Exception("Unknown types cannot be returned " +
                                                             "as query results: \n" +
                                                             s"""|Type: ${other.getClass.getSimpleName}
                                                                 |Value: $other""".stripMargin)
        }
}
