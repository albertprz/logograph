package orm

import java.sql.{Connection, ResultSet, PreparedStatement, Statement, Date, Time}
import java.math.BigDecimal
import scala.util.{Try, Success, Failure}
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.{universe => ru}
import utils.ReflectionUtils._
import utils.StringUtils._

class ScalaQLContext (conn: Connection) {

  import ScalaQLContext._

  conn.setAutoCommit(false)

  def run [T <: DbDataSet] (query: SelectStatement[T]) (implicit tag: ru.TypeTag[T]) =
    tryRun(query).get

  def tryRun [T <: DbDataSet] (query: SelectStatement[T]) (implicit tag: ru.TypeTag[T])  =
    runQuery(query)

  def run (stmts: SQLStatefulStatement*) =
    tryRun(stmts:_*).get

  def tryRun (stmts: SQLStatefulStatement*) = {
    runStatefulStatement(stmts)

  }

  private def runQuery [T <: DbDataSet] (query: SelectStatement[T]) (implicit tag: ru.TypeTag[T]) = Try {

    val companion = companionOf[T]

    val stmt = conn.prepareStatement(query.sql)
    parameteriseStatement(stmt, query.paramList)

    val resultSet = stmt.executeQuery()
    val results = ListBuffer[T]()

    while (resultSet.next()) {
      val ctorArgs = getCtorArgs(resultSet, companion.paramTypes)
      results += companion.apply(ctorArgs).asInstanceOf[T]
    }

    results.toList
  }

  private def runStatefulStatement  (stmts: Seq[SQLStatefulStatement], commit: Boolean = false) = Try {

    for ((sql, paramList) <- stmts.map(x => (x.sql, x.paramList))) {
      val stmt = conn.prepareStatement(sql)
      parameteriseStatement(stmt, paramList)
      stmt.execute()
    }

    if (commit) {
      conn.commit()
    }
  }
}

object ScalaQLContext {

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
        case other @ _      => throw new Exception("Unknown types cannot be used in queries " +
                                                    "for constant or runtime parameter values: \n" +
                                                    s"""|Type: ${other.getClass.getSimpleName}
                                                        |Value: $other""".stripMargin)
      }
    }

  private def getCtorArgs (resultSet: ResultSet, paramTypes: List[String]) =
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
