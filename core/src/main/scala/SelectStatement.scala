package com.albertoperez1994.scalaql

import config.ScalaQLConfig
import utils.TypeInfo
import utils.StringUtils.*
import utils.TreeUtils.*

import java.lang.reflect.ParameterizedType

case class SelectStatement [T <: DbDataSet] (sqlTemplate: String,
                                             params: Map[String, Any],
                                             tableNames: Seq[String],
                                             typeInfo: TypeInfo,
                                             subQueries: Seq[SelectStatement[DbDataSet]] = Seq.empty,
                                             var index: Int = 0,
                                             dependencies: Seq[Int] = Seq.empty)
                            extends SQLStatement {

  // .getActualTypeArguments()
    // .asInstanceOf[ParameterizedType]
    //                            .getActualTypeArguments()

  import SelectStatement._

  lazy val (sql, paramList) = SelectStatement.generate(this)

  lazy val validate = {}

  def run [F[+_]] () (implicit context: ScalaQLContext[F]) =
    context.run(this)

  def union (select: SelectStatement[T]) =
    SelectStatement.union(Seq(this, select))

  def unionAll (select: SelectStatement[T])  =
    SelectStatement.unionAll(Seq(this, select))

  def intersect (select: SelectStatement[T]) =
    SelectStatement.intersect(Seq(this, select))

  def except (select: SelectStatement[T]) =
    SelectStatement.except(Seq(this, select))

  def ++ (select: SelectStatement[T]) =
    this.union(select)

  def -- (select: SelectStatement[T]) =
    this.except(select)

  def && (select: SelectStatement[T]) =
    this.intersect(select)

  override def toString () =
    s"Query: \n\n$sqlTemplate \n\nParams:  \n\n${pprint(params)}\n\n"
}

case object SelectStatement {

  import SQLStatement.*

  given ScalaQLConfig = ScalaQLConfig.get

  def union[T <: DbDataSet] (selects: Seq[SelectStatement[T]]) =
    concat(selects, "UNION")

  def unionAll[T <: DbDataSet] (selects: Seq[SelectStatement[T]]) =
    concat(selects, "UNION ALL")

  def intersect[T <: DbDataSet] (selects: Seq[SelectStatement[T]]) =
    concat(selects, "INTERSECT")

  def except[T <: DbDataSet] (selects: Seq[SelectStatement[T]]) =
    concat(selects, "EXCEPT")


  private def concat[T <: DbDataSet] (selects: Seq[SelectStatement[T]], separator: String) = {

    val sqlTemplate = selects.map(_.sql)
                             .mkString("\n" + separator + "\n\n")
    val params = selects.map(_.params)
                        .fold(Map.empty)(_++_)

    val tableNames = selects.map(_.tableNames)
                              .fold(Seq.empty)(_++_)

    val typeInfo = selects.head.typeInfo

    SelectStatement[T](sqlTemplate, params, tableNames, typeInfo)
  }

  private def generate [T <: DbDataSet] (select: SelectStatement[?]) =  {

    select.validate

    val queries = buildTree(select.asInstanceOf[SelectStatement[DbDataSet]]) (_.subQueries)
      .foldLeftWithIndex(Seq.empty[SelectStatement[DbDataSet]]) {
        case ((acc, i), curr) => {
          curr.index = i;
          acc :+ curr.copy(dependencies = curr.subQueries.map(_.index).toSeq)
        }
      }
      .map { select =>
        ((select.tableNames zip select.dependencies)
          .foldLeft(select.sqlTemplate) {
            case (querySql, (tableName, i)) => querySql.replaceFirst(s"\\[${tableName.unwrap()}\\]", s"[q$i]")
          }, select.index)
      }

    val ctes = queries.init
                      .map{ case (querySql, i) => s"q$i AS\n(\n${querySql.indent(2)})" }
                      .mkString("WITH ", ",\n", "\n")

    val (finalQuery, _) = queries.last

    val sqlTemplate = if (queries.init.nonEmpty) s"$ctes\n$finalQuery"
                      else finalQuery

    val params = (select.subQueries.map(_.params) :+ select.params).flatten.toMap

    (getSQL(sqlTemplate, params), getParamList(params))
  }
}
