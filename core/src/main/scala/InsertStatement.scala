package com.albertoperez1994.scalaql


import com.albertoperez1994.scalaql.config.ScalaQLConfig
import com.albertoperez1994.scalaql.core.{Table, Column}
import com.albertoperez1994.scalaql.utils
import utils.StringUtils._
import utils.TypeInfo
import scala.deriving.Mirror

case class InsertStatement [T <: DbTable] (data: Either[Seq[T], SelectStatement[T]], typeInfo: TypeInfo)
    extends SQLStatefulStatement {

  lazy val (sql, paramList) = InsertStatement.generate(this)

  lazy val validate = {}

  def run [F[+_]] () (implicit context: ScalaQLContext[F]) =
    context.run(this)

  override def toString () =
    data match {
      case Left(data)   => s"""Insert Statement: \n\n${sql.substring(0, sql.indexOf("?)")) + "?)"}\n\n"""
      case Right(query) => s"""Insert Statement: \n\n$sql \n\nParams: \n\n${pprint(query.paramList)}\n\n"""
    }
}

object InsertStatement {

  given ScalaQLConfig = ScalaQLConfig.get

  def generate [T <: DbTable] (insert: InsertStatement[T]) = {

    import insert.{data, typeInfo}

    val sql = insert.data match {
      case Left(data) => getSQL(data, typeInfo)
      case Right(query) => getSQL(query, typeInfo)
    }

    val paramList = insert.data match {
      case Left(data) => data.flatMap(_.productIterator).toList
      case Right(query) => query.paramList
    }

    (sql, paramList)
  }


  def getSQL [T <: DbTable] (data: Seq[T], typeInfo: TypeInfo) = {

    val tableName = typeInfo.className
    val table = Table(tableName)

    val elemNames = typeInfo.elemNames
    val paramPlaceholders = (0 to data.size - 1).map(x => elemNames.map(x => "?"))
                                                .map(stringify)
                                                .mkString(", \n")

     s"""|INSERT INTO ${table.sql} ${stringify(elemNames.map(Column(_, tableName).sql))}
         |VALUES      $paramPlaceholders """.stripMargin
  }

  def getSQL [T <: DbTable] (query: SelectStatement[T], typeInfo: TypeInfo) = {

    val tableName = typeInfo.className
    val table = Table(tableName)

    val elemNames = typeInfo.elemNames

    s"""|INSERT INTO ${table.sql} ${stringify(elemNames.map(Column(_, tableName).sql))}
        |${query.sql} """.stripMargin
  }
}
