package orm

import java.sql.{Connection, ResultSet, PreparedStatement, Statement, Date, Time}
import java.math.BigDecimal
import scala.util.Try
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.{universe => ru}
import utils.ReflectionUtils._
import utils.StringUtils._

class ScalaQLContext (conn: Connection) {

  import ScalaQLContext._

  conn.setAutoCommit(false)

  def run [T <: DbDataSet] (query: SelectQuery[T]) (implicit tag: ru.TypeTag[T]) =
    tryRun(query).get

  def tryRun [T <: DbDataSet] (query: SelectQuery[T]) (implicit tag: ru.TypeTag[T])  =
    runQuery(query)

  def run (inserts: InsertStatement[_]*) =
    tryRun(inserts:_*).get

  def tryRun (inserts: InsertStatement[_]*) =
    runInsertBatch(inserts)

  def run (deletes: DeleteStatement[_]*) (implicit d: DummyImplicit) =
    tryRun(deletes:_*)(d).get

  def tryRun (deletes: DeleteStatement[_]*) (implicit d: DummyImplicit) =
    runDeleteBatch(deletes)

  def run (updates: UpdateStatement[_]*) (implicit d1: DummyImplicit, d2: DummyImplicit) =
    tryRun(updates:_*)(d1, d2).get

  def tryRun (updates: UpdateStatement[_]*) (implicit d1: DummyImplicit, d2: DummyImplicit) =
    runUpdateBatch(updates)


  private def runQuery [T <: DbDataSet] (query: SelectQuery[T]) (implicit tag: ru.TypeTag[T]) = Try {

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

  private def runInsertBatch  (inserts: Seq[InsertStatement[_]]) = Try {

    for ((sql, paramList) <- inserts.map(x => (x.sql, x.paramList))) {
      val stmt = conn.prepareStatement(sql)
      parameteriseStatement(stmt, paramList)
      stmt.execute()
    }

    conn.commit()
  }

  private def runDeleteBatch (deletes: Seq[DeleteStatement[_]]) = Try {

    for (sql <- deletes map (_.sql)) {
      val stmt = conn.createStatement()
      stmt.execute(sql)
    }

    conn.commit()
  }

  private def runUpdateBatch (updates: Seq[UpdateStatement[_]]) = Try {

    for (sql <- updates map (_.sql)) {
      val stmt = conn.createStatement()
      stmt.execute(sql)
    }

    conn.commit()
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