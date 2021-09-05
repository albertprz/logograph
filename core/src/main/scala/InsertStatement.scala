package com.albertoperez1994.scalaql

import scala.reflect.runtime.{universe => ru}

import com.albertoperez1994.scalaql.utils
import utils.StringUtils._
import utils.ReflectionUtils._

case class InsertStatement [T <: DbTable] (data: Either[Seq[T], SelectStatement[T]])
                                            (implicit tag: ru.TypeTag[T])
                                            extends SQLStatefulStatement {

  lazy val (sql, paramList) = InsertStatement.generate(this)

  def run [F[+_]] () (implicit context: ScalaQLContext[F]) =
    context.run(this)

  override def toString () =
    data match {
      case Left(data)   => s"""Insert Statement: \n\n${sql.substring(0, sql.indexOf("?)")) + "?)"}\n\n"""
      case Right(query) => s"""Insert Statement: \n\n$sql \n\nParams: \n\n${pprint(query.paramList)}\n\n"""
    }
}

private object InsertStatement {

  def generate [T <: DbTable] (insert: InsertStatement[T])
                              (implicit tag: ru.TypeTag[T]) = {

    val sql = insert.data match {
      case Left(data) => getSQL(data)
      case Right(query) => getSQL(query)
    }

    val paramList = insert.data match {
      case Left(data) => data.flatMap(_.productIterator).toList
      case Right(query) => query.paramList
    }

    (sql, paramList)
  }


  def getSQL [T <: DbTable] (data: Seq[T]) (implicit tag: ru.TypeTag[T]) = {

    val tableName = className[T]
    val companion = companionOf[T]
    val paramNames = companion.paramNames.map(x => s"[$x]")
    val paramPlaceholders = (0 to data.size - 1).map(x => paramNames.map(x => "?"))
                                                .map(stringify)
                                                .mkString(", \n")

     s"""|INSERT INTO [$tableName] ${stringify(paramNames)}
         |VALUES      $paramPlaceholders """.stripMargin
  }

  def getSQL [T <: DbTable] (query: SelectStatement[T]) (implicit tag: ru.TypeTag[T]) = {

    val tableName = className[T]
    val companion = companionOf[T]
    val paramNames = companion.paramNames.map(x => s"[$x]")

    s"""|INSERT INTO [$tableName] ${stringify(paramNames)}
        |${query.sql} """.stripMargin
  }
}
