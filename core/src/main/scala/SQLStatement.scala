package orm

import scala.reflect.runtime.{universe => ru}
import utils.QueryUtils
import utils.ReflectionUtils._
import utils.StringUtils._

sealed trait SQLStatement {
  val sql: String
  val paramList: List[Any]
  def run () (implicit context: ScalaQLContext): Any
  def tryRun () (implicit context: ScalaQLContext): Any
}

sealed trait SQLStatefulStatement extends SQLStatement

case class SelectStatement [T <: DbDataSet] (private val sqlTemplate: String,
                                             private val params: Map[String, Any])
                                            (implicit tag: ru.TypeTag[T]) extends SQLStatement {

  import SQLStatement._

  val sql = getSQL(sqlTemplate, params)
  val paramList = getParamList(params)

  def run () (implicit context: ScalaQLContext) =
    context.run(this)

  def tryRun () (implicit context: ScalaQLContext) =
    context.tryRun(this)

  override def toString () =
    s"Query: \n\n${sql} \n\nParams:  \n\n${pprint(params)}\n\n"
}

case class InsertStatement [T <: DbTable] (private val data: Either[Seq[T], SelectStatement[T]])
                                            (implicit tag: ru.TypeTag[T])
                                            extends SQLStatefulStatement {

  val (sql, paramList) = data match {
    case Left(data) => getSQL(data)
    case Right(query) => getSQL(query)
  }

  def run () (implicit context: ScalaQLContext) =
    context.run(this)

  def tryRun () (implicit context: ScalaQLContext) =
    context.tryRun(this)


  private def getSQL [T <: DbTable] (data: Seq[T]) (implicit tag: ru.TypeTag[T]) = {

    val companion = companionOf[T]
    val tableName = className[T]
    val paramNames = companion.paramNames
    val paramPlaceholders = (0 to data.size - 1).map(x => paramNames.map(x => "?"))
                                                .map(stringify)
                                                .mkString(", \n")

    val sql = s"""|INSERT INTO [$tableName] ${stringify(paramNames)}
                  |VALUES $paramPlaceholders """.stripMargin

    val paramList = data.flatMap(_.productIterator).toList

    (sql, paramList)
  }

  private def getSQL [T <: DbTable] (query: SelectStatement[T]) (implicit tag: ru.TypeTag[T]) = {

    val tableName = className[T]
    val companion = companionOf[T]
    val paramNames = stringify(companion.paramNames)

    val sql = s"""|INSERT INTO [$tableName] $paramNames
                  |${query.sql} """.stripMargin

    (sql, query.paramList)
  }

  override def toString () =
    s"Insert Statement: \n\n${sql} \n\n"
}

case class DeleteStatement [T <: DbTable]  (private val sqlTemplate: String,
                                            private val params: Map[String, Any] = Map.empty)
                                           extends SQLStatefulStatement {
  import SQLStatement._

  val sql = getSQL(sqlTemplate, params)
  val paramList = getParamList(params)

  def run () (implicit context: ScalaQLContext) =
    context.run(this)

  def tryRun () (implicit context: ScalaQLContext) =
    context.tryRun(this)

  override def toString () =
    s"Delete Statement: \n\n${sql} \n\nParams:  \n\n${pprint(params)}\n\n"
}

case class UpdateStatement [T <: DbTable] (private val sqlTemplate: String,
                                           private val params: Map[String, Any] = Map.empty)
                                          extends SQLStatefulStatement {

  import SQLStatement._

  val sql = getSQL(sqlTemplate, params)
  val paramList = getParamList(params)

  def run () (implicit context: ScalaQLContext) =
    context.run(this)

  def tryRun () (implicit context: ScalaQLContext) =
    context.tryRun(this)

  override def toString () =
    s"Update Statement: \n\n${sql} \n\nParams:  \n\n${pprint(params)}\n\n"
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
