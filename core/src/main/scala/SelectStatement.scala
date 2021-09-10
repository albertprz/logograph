package com.albertoperez1994.scalaql

import scala.reflect.runtime.{universe => ru}

import com.albertoperez1994.scalaql.config.ScalaQLConfig
import com.albertoperez1994.scalaql.utils
import utils.StringUtils._
import utils.TreeUtils._

case class SelectStatement [T <: DbDataSet] (sqlTemplate: String,
                                             params: Map[String, Any],
                                             tableNames: Seq[String],
                                             subQueries: Seq[SelectStatement[DbDataSet]] = Seq.empty,
                                             var index: Int = 0,
                                             dependencies: Seq[Int] = Seq.empty)
                           (implicit tag: ru.TypeTag[T]) extends SQLStatement {

  lazy val (sql, paramList) = SelectStatement.generate(this)

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

  def ++ (select: SelectStatement[T]) (implicit tag: ru.TypeTag[T]) =
    this.union(select)

  def -- (select: SelectStatement[T]) (implicit tag: ru.TypeTag[T]) =
    this.except(select)

  def && (select: SelectStatement[T]) (implicit tag: ru.TypeTag[T]) =
    this.intersect(select)

  override def toString () =
    s"Query: \n\n$sqlTemplate \n\nParams:  \n\n${pprint(params)}\n\n"
}

object SelectStatement {

  import SQLStatement._

  private implicit val cfg = ScalaQLConfig.get

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

    val sqlTemplate = selects.map(_.sql)
                             .mkString("\n" + separator + "\n\n")
    val params = selects.map(_.params)
                        .fold(Map.empty)(_++_)

    val tableNames = selects.map(_.tableNames)
                              .fold(Seq.empty)(_++_)

    SelectStatement[T](sqlTemplate, params, tableNames)
  }

  private def generate [T <: DbDataSet] (select: SelectStatement[_]) =  {

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
            case (querySql, (tableName, i)) => querySql.replaceFirst(s"\\[$tableName\\]", s"[q$i]")
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
