package orm

import java.sql.{Connection, ResultSet, Statement}
import scala.util.Try
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.{universe => ru}
import utils.ReflectionUtils

class ScalaQLContext (conn: Connection) {


  def run [T: ru.TypeTag] (query: FullQuery[_, T]) =
    runQuery(query).get

  private def runQuery [T: ru.TypeTag] (query: FullQuery[_, T]) = Try {

    val companion = ReflectionUtils.companionOf[T]
    val stmt = conn.createStatement
    val resultSet = stmt.executeQuery(query.sql)

    val results = ListBuffer[T]()

    while (resultSet.next()) {
      val ctorArgs = getCtorArgs(resultSet, companion.paramCount, companion.paramTypes)
      results += companion.apply(ctorArgs).asInstanceOf[T]
    }

    results.toList
  }


  private def getCtorArgs(resultSet: ResultSet, paramCount: Int, paramTypes: Array[String]) =
    for (i <- 0 to paramCount - 1)
        yield paramTypes(i) match {
          case "int" => resultSet.getInt(i + 1)
          case "boolean" => resultSet.getBoolean(i + 1)
          case "java.lang.String" => resultSet.getString(i + 1)
          case _ => null
        }
}
