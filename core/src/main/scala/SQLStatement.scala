package com.albertoperez1994.scalaql

import scala.reflect.runtime.{universe => ru}

import com.albertoperez1994.scalaql.{utils => utils}
import utils.QueryUtils
import utils.ReflectionUtils._
import utils.StringUtils._

sealed trait SQLStatement {
  val sql: String
  val paramList: List[Any]
  def run [F[+_]] () (implicit context: ScalaQLContext[F]): F[Any]
}

sealed trait SQLStatefulStatement extends SQLStatement

case class SelectStatement [T <: DbDataSet] (private val sqlTemplate: String,
                                             private val params: Map[String, Any])
                                            (implicit tag: ru.TypeTag[T]) extends SQLStatement {

  import SQLStatement._

  val sql = getSQL(sqlTemplate, params)
  val paramList = getParamList(params)

  def run [F[+_]] () (implicit context: ScalaQLContext[F]) =
    context.run(this)

  def union (select: SelectStatement[T]) (implicit tag: ru.TypeTag[T]) =
    SelectStatement.union(Seq(this, select))

  def unionAll (select: SelectStatement[T]) (implicit tag: ru.TypeTag[T]) =
    SelectStatement.unionAll(Seq(this, select))

  def intersect (select: SelectStatement[T]) (implicit tag: ru.TypeTag[T]) =
    SelectStatement.intersect(Seq(this, select))

  def except (select: SelectStatement[T]) (implicit tag: ru.TypeTag[T]) =
    SelectStatement.except(Seq(this, select))


  override def toString () =
    s"Query: \n\n$sqlTemplate \n\nParams:  \n\n${pprint(params)}\n\n"
}

object SelectStatement {

  def union[T <: DbDataSet] (selects: Seq[SelectStatement[T]]) (implicit tag: ru.TypeTag[T]) =
    concat(selects, "UNION")

  def unionAll[T <: DbDataSet] (selects: Seq[SelectStatement[T]]) (implicit tag: ru.TypeTag[T]) =
    concat(selects, "UNION ALL")

  def intersect[T <: DbDataSet] (selects: Seq[SelectStatement[T]]) (implicit tag: ru.TypeTag[T]) =
    concat(selects, "INTERSECT")

  def except[T <: DbDataSet] (selects: Seq[SelectStatement[T]]) (implicit tag: ru.TypeTag[T]) =
    concat(selects, "EXCEPT")


  private def concat[T <: DbDataSet] (selects: Seq[SelectStatement[T]], separator: String)
                                     (implicit tag: ru.TypeTag[T]) = {

    val sqlTemplate = selects.map(_.sqlTemplate)
                             .mkString("\n" + separator + "\n\n")
    val params = selects.map(_.params)
                        .fold(Map.empty)(_++_)

    SelectStatement[T](sqlTemplate, params)
  }

}

case class InsertStatement [T <: DbTable] (private val data: Either[Seq[T], SelectStatement[T]])
                                            (implicit tag: ru.TypeTag[T])
                                            extends SQLStatefulStatement {

  val (sql, paramList) = data match {
    case Left(data) => getSQL(data)
    case Right(query) => getSQL(query)
  }

  def run [F[+_]] () (implicit context: ScalaQLContext[F]) =
    context.run(this)


  private def getSQL [T <: DbTable] (data: Seq[T]) (implicit tag: ru.TypeTag[T]) = {

    val tableName = className[T]
    val companion = companionOf[T]
    val paramNames = companion.paramNames.map(x => s"[$x]")
    val paramPlaceholders = (0 to data.size - 1).map(x => paramNames.map(x => "?"))
                                                .map(stringify)
                                                .mkString(", \n")

    val sql = s"""|INSERT INTO [$tableName] ${stringify(paramNames)}
                  |VALUES      $paramPlaceholders """.stripMargin

    val paramList = data.flatMap(_.productIterator).toList

    (sql, paramList)
  }

  private def getSQL [T <: DbTable] (query: SelectStatement[T]) (implicit tag: ru.TypeTag[T]) = {

    val tableName = className[T]
    val companion = companionOf[T]
    val paramNames = companion.paramNames.map(x => s"[$x]")

    val sql = s"""|INSERT INTO [$tableName] ${stringify(paramNames)}
                  |${query.sql} """.stripMargin

    (sql, query.paramList)
  }

  override def toString () =
    data match {
      case Left(data)   => s"""Insert Statement: \n\n${sql.substring(0, sql.indexOf("?)")) + "?)"}\n\n"""
      case Right(query) => s"""Insert Statement: \n\n$sql \n\nParams: \n\n${pprint(query.paramList)}\n\n"""
    }
}

case class DeleteStatement [T <: DbTable]  (private val sqlTemplate: String,
                                            private val params: Map[String, Any] = Map.empty)
                                           extends SQLStatefulStatement {
  import SQLStatement._

  val sql = getSQL(sqlTemplate, params)
  val paramList = getParamList(params)

  def run [F[+_]] () (implicit context: ScalaQLContext[F]) =
    context.run(this)

  override def toString () =
    s"Delete Statement: \n\n$sqlTemplate \n\nParams:  \n\n${pprint(params)}\n\n"
}

case class UpdateStatement [T <: DbTable] (private val sqlTemplate: String,
                                           private val params: Map[String, Any] = Map.empty)
                                          extends SQLStatefulStatement {

  import SQLStatement._

  val sql = getSQL(sqlTemplate, params)
  val paramList = getParamList(params)

  def run [F[+_]] () (implicit context: ScalaQLContext[F]) =
    context.run(this)

  override def toString () =
    s"Update Statement: \n\n$sqlTemplate \n\nParams:  \n\n${pprint(params)}\n\n"
}

object SQLStatement {

  def getParamList (params: Map[String, Any]) =
    (params.values flatMap { _ match {
        case list: List[Any] => list
        case other: Any => List(other)
      }
    }).toList


  def getSQL (sqlTemplate: String, params: Map[String, Any]) =
    params.values
          .filter (_.isInstanceOf[List[Any]])
          .foldLeft (sqlTemplate) {
            case (acc, curr: List[Any]) => acc.replaceFirst("in [?]", curr.map(x => "?")
                                                                          .mkString("in (", ", ", ")"))
          }
}
