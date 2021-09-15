package com.albertoperez1994.scalaql

import java.sql.{Connection, ResultSet, PreparedStatement, Statement, Date, Time}
import java.math.BigDecimal
import scala.util.{Try, Success, Failure}
import scala.collection.mutable.ListBuffer
import cats.Monad
import cats.syntax.all.*
import cats.effect.{Sync, Resource}

import com.albertoperez1994.scalaql.config.ScalaQLConfig
import utils.TypeInfo
import utils.StringUtils.*
import java.lang.reflect.Method

class ScalaQLContext [F[_] : Sync : Monad] (conn: Connection) {

  import ScalaQLContext._

  conn.setAutoCommit(false)

  def run [T <: DbDataSet] (query: SelectStatement[T]): F[List[T]]  =
    runQuery (query)

  def run (stmts: SQLStatefulStatement*): F[Unit] =
    runStatefulStatement (stmts)


  private def runQuery [T <: DbDataSet] (query: SelectStatement[T]): F[List[T]] =
    prepareStatement(query.sql, query.paramList).use { stmt =>

      for (resultSet <- Sync[F].blocking { stmt.executeQuery() })
        yield extractResults(resultSet, query.typeInfo)
    }

  private def runStatefulStatement  (stmts: Seq[SQLStatefulStatement]) = {

    val preparedStmts = for ((sql, paramList) <- stmts.map(x => (x.sql, x.paramList)))
                        yield prepareStatement(sql, paramList)
                                .use { stmt => Sync[F].blocking { stmt.executeUpdate() } }

    preparedStmts.fold (Sync[F].pure(0)) { case (acc, curr) => acc *> curr } *>
    Sync[F].blocking { conn.commit() }
  }

  private def prepareStatement (querySql: String, paramList: List[Any]) =
    Resource.make { Sync[F].blocking { conn.prepareStatement(querySql) } }
                  { stmt => Sync[F].blocking { stmt.close() } }
            .map  { stmt => parameteriseStatement(stmt, paramList) }
}

private object ScalaQLContext {

  def extractResults [T <: DbDataSet] (resultSet: ResultSet, typeInfo: TypeInfo) = {

    val results = ListBuffer[T]()

    val constructor = Class.forName(typeInfo.fullClassName)
                           .getConstructors()
                           .head


    while (resultSet.next()) {
      val ctorArgs = getCtorArgs(resultSet, typeInfo.elemTypes)
      results += constructor.newInstance(ctorArgs: _*).asInstanceOf[T]
    }

    results.toList
  }

  def parameteriseStatement (stmt: PreparedStatement, params: List[Any]) = {
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
                                                    s"""|Type: ${other.getClass.getName()}
                                                        |Value: $other""".stripMargin)
      }
    }
    stmt
  }

  def getCtorArgs (resultSet: ResultSet, paramTypes: List[String]) =
    for (i <- 0 to paramTypes.size - 1)
        yield paramTypes(i) match {
          case "Int"            => resultSet.getInt(i + 1)
          case "Long"           => resultSet.getLong(i + 1)
          case "Float"          => resultSet.getFloat(i + 1)
          case "Double"         => resultSet.getDouble(i + 1)
          case "Boolean"        => resultSet.getBoolean(i + 1)
          case "String"         => resultSet.getString(i + 1)
          case "BigDecimal"     => resultSet.getBigDecimal(i + 1)
          case "date"           => resultSet.getDate(i + 1)
          case "time"           => resultSet.getTime(i + 1)
          case other @ _        => throw new Exception("Unknown types cannot be returned " +
                                                        "as query results: \n" +
                                                        s"Type: ${other.getClass.getName()}".stripMargin)
        }
}
