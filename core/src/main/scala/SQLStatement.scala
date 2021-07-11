package com.albertoperez1994.scalaql

import scala.reflect.runtime.{universe => ru}

import com.albertoperez1994.scalaql.{utils => utils}
import utils.QueryUtils
import utils.ReflectionUtils._
import utils.StringUtils._
import com.albertoperez1994.scalaql.core.QueryClause
import com.albertoperez1994.scalaql.core.UpdateClause
import com.albertoperez1994.scalaql.core.DeleteClause

sealed trait SQLStatement {
  def sql() (implicit cfg: DbConfig): String
  def paramList(): List[Any]
  def run [F[+_]] () (implicit context: ScalaQLContext[F]): F[Any]
}

sealed trait SQLStatefulStatement extends SQLStatement

case class SelectStatement [T <: DbDataSet] (private val clause: QueryClause,
                                             private val params: Map[String, Any])
                           (implicit tag: ru.TypeTag[T]) extends SQLStatement {

  import SQLStatement._

  def sql() (implicit cfg: DbConfig): String = getSQL(clause.sql, params)
  def paramList() = getParamList(params)

  def run [F[+_]] () (implicit context: ScalaQLContext[F]) =
    context.run(this)

  // def union (select: SelectStatement[T]) (implicit tag: ru.TypeTag[T]) =
  //   SelectStatement.union(Seq(this, select))

  // def unionAll (select: SelectStatement[T]) (implicit tag: ru.TypeTag[T]) =
  //   SelectStatement.unionAll(Seq(this, select))

  // def intersect (select: SelectStatement[T]) (implicit tag: ru.TypeTag[T]) =
  //   SelectStatement.intersect(Seq(this, select))

  // def except (select: SelectStatement[T]) (implicit tag: ru.TypeTag[T]) =
  //   SelectStatement.except(Seq(this, select))


  // override def toString () =
  //   s"Query: \n\n$sqlTemplate \n\nParams:  \n\n${pprint(params)}\n\n"
}

// object SelectStatement {

//   def union[T <: DbDataSet] (selects: Seq[SelectStatement[T]]) (implicit tag: ru.TypeTag[T]) =
//     concat(selects, "UNION")

//   def unionAll[T <: DbDataSet] (selects: Seq[SelectStatement[T]]) (implicit tag: ru.TypeTag[T]) =
//     concat(selects, "UNION ALL")

//   def intersect[T <: DbDataSet] (selects: Seq[SelectStatement[T]]) (implicit tag: ru.TypeTag[T]) =
//     concat(selects, "INTERSECT")

//   def except[T <: DbDataSet] (selects: Seq[SelectStatement[T]]) (implicit tag: ru.TypeTag[T]) =
//     concat(selects, "EXCEPT")


//   private def concat[T <: DbDataSet] (selects: Seq[SelectStatement[T]], separator: String)
//                     (implicit tag: ru.TypeTag[T],
//                     cfg: DbConfig) = {

//     val sqlTemplate = selects.map(_.sql)
//                              .mkString("\n" + separator + "\n\n")
//     val params = selects.map(_.params)
//                         .fold(Map.empty)(_++_)

//     SelectStatement[T](sqlTemplate, params)
//   }

// }

case class InsertStatement [T <: DbTable] (private val data: Either[Seq[T], SelectStatement[T]])
                                            (implicit tag: ru.TypeTag[T])
                                            extends SQLStatefulStatement {

  def sql()(implicit cfg: DbConfig) = data match {
    case Left(data) => getSQL(data)
    case Right(query) => getSQL(query)
  }

  def paramList() = data match {
    case Left(data) => data.flatMap(_.productIterator).toList
    case Right(query) => query.paramList
  }


  def run [F[+_]] () (implicit context: ScalaQLContext[F]) =
    context.run(this)


  private def getSQL [T <: DbTable] (data: Seq[T]) (implicit tag: ru.TypeTag[T],
  cfg: DbConfig) = {

    val tableName = className[T]
    val companion = companionOf[T]
    val paramNames = companion.paramNames.map(x => s"[$x]")
    val paramPlaceholders = (0 to data.size - 1).map(x => paramNames.map(x => "?"))
                                                .map(stringify)
                                                .mkString(", \n")

     s"""|INSERT INTO [$tableName] ${stringify(paramNames)}
         |VALUES      $paramPlaceholders """.stripMargin
  }

  private def getSQL [T <: DbTable] (query: SelectStatement[T]) (implicit tag: ru.TypeTag[T],
  cfg: DbConfig) = {

    val tableName = className[T]
    val companion = companionOf[T]
    val paramNames = companion.paramNames.map(x => s"[$x]")

    s"""|INSERT INTO [$tableName] ${stringify(paramNames)}
        |${query.sql} """.stripMargin
  }

  // override def toString () (implicit cfg: DbConfig) =
  //   data match {
  //     case Left(data)   => s"""Insert Statement: \n\n${sql.substring(0, sql.indexOf("?)")) + "?)"}\n\n"""
  //     case Right(query) => s"""Insert Statement: \n\n$sql \n\nParams: \n\n${pprint(query.paramList)}\n\n"""
  //   }
}

case class DeleteStatement [T <: DbTable]  (private val clause: DeleteClause,
                                            private val params: Map[String, Any] = Map.empty)
                                           extends SQLStatefulStatement {
  import SQLStatement._

  def sql() (implicit cfg: DbConfig) = getSQL(clause.sql, params)
  def paramList() = getParamList(params)

  def run [F[+_]] () (implicit context: ScalaQLContext[F]) =
    context.run(this)

  // override def toString () =
  //   s"Delete Statement: \n\n$sqlTemplate \n\nParams:  \n\n${pprint(params)}\n\n"
}

case class UpdateStatement [T <: DbTable] (private val clause: UpdateClause,
                                           private val params: Map[String, Any] = Map.empty)
                                          extends SQLStatefulStatement {

  import SQLStatement._

  def sql() (implicit cfg: DbConfig) = getSQL(clause.sql, params)
  def paramList() = getParamList(params)

  def run [F[+_]] () (implicit context: ScalaQLContext[F]) =
    context.run(this)

  // override def toString () =
  //   s"Update Statement: \n\n$sqlTemplate \n\nParams:  \n\n${pprint(params)}\n\n"
}

object SQLStatement {

  def getParamList (params: Map[String, Any]) =
    (params.values flatMap { _ match {
        case list: List[Any] => list
        case other: Any => List(other)
      }
    }).toList


  def getSQL (sqlTemplate: String, params: Map[String, Any]) (implicit cfg: DbConfig) =
    params.values
          .filter (_.isInstanceOf[List[Any]])
          .foldLeft (sqlTemplate) {
            case (acc, curr: List[Any]) => acc.replaceFirst("in [?]", curr.map(x => "?")
                                                                          .mkString("in (", ", ", ")"))
          }
}
