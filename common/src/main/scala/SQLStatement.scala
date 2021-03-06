package orm

import scala.reflect.runtime.{universe => ru}
import utils.QueryUtils
import utils.ReflectionUtils._
import utils.StringUtils._

sealed abstract class SQLStatement

case class SelectQuery [T, R] (private val query: Option[T ⇒ Query[R]] = None,
                               private val subqueries: Seq[SelectQuery[_, _]] = Seq.empty,
                               private val queryClauseSql: Option[String] = None,
                               val params: Map[String, Any] = Map.empty)
                               extends SQLStatement {

  private val querySql =
    params.values
              .filter(_.isInstanceOf[List[Any]])
              .foldLeft(queryClauseSql.getOrElse("")) {
                case (acc, curr: List[Any]) => acc.replaceFirst("in [?]", curr.map(x => "?")
                                                                              .mkString("in (", ", ", ")"))
    }

  val sql = querySql

  val paramList = (params.values flatMap { _ match {
        case list: List[Any] => list
        case other: Any => List(other)
      }
    }).toList
}

case class InsertStatement [T <: DbTable] (val data: Either[Seq[T], SelectQuery[_, T]])
                                            (implicit tag: ru.TypeTag[T])
                                            extends SQLStatement {

  val (sql, paramList) = data match {

    case Left(data) => getSQL(data)
    case Right(query) => getSQL(query)
  }

  private def getSQL [T <: DbTable] (data: Seq[T]) (implicit tag: ru.TypeTag[T]) = {

    val companion = companionOf[T]
    val tableName = companion.className
    val paramNames = companion.paramNames
    val paramPlaceholders = (0 to data.size - 1).map(x => paramNames.map(x => "?"))
                                                .map(stringify)
                                                .mkString(", \n")

    val sql = s"""|INSERT INTO [$tableName] ${stringify(paramNames)}
                  |VALUES $paramPlaceholders """.stripMargin

    val paramList = data.flatMap(_.productIterator).toList

    (sql, paramList)
  }


  private def getSQL [T <: DbTable] (query: SelectQuery[_, T]) (implicit tag: ru.TypeTag[T]) = {

    val companion = companionOf[T]
    val tableName = companion.className
    val paramNames = stringify(companion.paramNames)

    val sql = s"""|INSERT INTO [$tableName] $paramNames
                  |${query.sql} """.stripMargin

    (sql, query.paramList)
  }
}
