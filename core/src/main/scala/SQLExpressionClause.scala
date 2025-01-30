package com.albertprz.logograph.core

import com.albertprz.logograph.utils.Error.*
import com.albertprz.logograph.utils.StringUtils.*
import com.albertprz.logograph.config.LogographConfig
import cats.Foldable
import cats.syntax.all.*
import SQLExpressionClause.*

trait SQLExpressionClause extends SQLClause:

  def exprs: List[SQLExpression]

trait BaseSelectClause extends SQLExpressionClause

case class SelectClause(
    exprs: List[SQLExpression],
    columnAliases: List[Column]
)(using LogographConfig)
    extends BaseSelectClause:

  lazy val sql =
    exprs
      .zip(columnAliases)
      .map { case (expr, alias) => s"${expr.sql} AS ${alias.sql}" }
      .mkString("SELECT      ", ", ", "\n")

case class SelectDistinctClause(
    exprs: List[SQLExpression],
    columnAliases: List[Column]
)(using LogographConfig)
    extends BaseSelectClause:

  lazy val sql =
    exprs
      .zip(columnAliases)
      .map { case (expr, alias) => s"${expr.sql} AS ${alias.sql}" }
      .mkString("SELECT      DISTINCT ", ", ", "\n")

case class SelectAllClause(tableAlias: String)(using LogographConfig)
    extends BaseSelectClause:

  lazy val exprs = List.empty

  lazy val sql = s"SELECT      $tableAlias.*\n"

case class SelectDistinctAllClause(tableAlias: String)(using LogographConfig)
    extends BaseSelectClause:

  lazy val exprs = List.empty

  lazy val sql = s"SELECT      DISTINCT $tableAlias.*\n"

case class FromClause(tableAliasesMap: Map[String, Table])(using
    LogographConfig
) extends SQLExpressionClause:

  lazy val exprs = List.empty

  lazy val sql =
    tableAliasesMap
      .map { case (tableAlias, table) =>
        s"${table.sql} AS $tableAlias"
      }
      .mkString("FROM        ", ", ", "\n")

case class WhereClause(exprs: List[SQLExpression])(using LogographConfig)
    extends SQLExpressionClause:

  lazy val sql =
    exprs
      .map(Predicate.adaptSql)
      .map(str => if exprs.size > 1 then str.wrapParens() else str)
      .mkString("WHERE       ", s" ${Operator("and").sql} \n            ", "\n")

case class GroupByClause(fields: List[Field])(using LogographConfig)
    extends SQLExpressionClause:

  lazy val exprs = fields

  lazy val sql =
    fields
      .map(_.sql)
      .mkString("GROUP BY    ", ", ", "\n")

case class HavingClause(exprs: List[SQLExpression])(using LogographConfig)
    extends SQLExpressionClause:

  lazy val sql =
    exprs
      .map(Predicate.adaptSql)
      .map(str => if exprs.size > 1 then str.wrapParens() else str)
      .mkString("HAVING      ", s" ${Operator("and").sql} \n            ", "\n")

case class OrderByClause(exprs: List[SQLExpression])(using LogographConfig)
    extends SQLExpressionClause:

  override lazy val validate =

    if exprs.exists(x => !(x.isInstanceOf[Field] || x.isInstanceOf[Operation]))
    then Some(InvalidExpression())
    else None

  lazy val sql =
    exprs
      .map(_.sql)
      .mkString("ORDER BY    ", ", ", "\n")

sealed abstract class BaseJoinClause(joinType: String)(using LogographConfig)
    extends SQLExpressionClause:

  val table: Table
  val tableAlias: String
  def exprs: List[SQLExpression]

  lazy val sql =
    exprs
      .map(Predicate.adaptSql)
      .map(str => if exprs.size > 1 then str.wrapParens() else str)
      .mkString(
        s"$joinType  ${table.sql} AS $tableAlias ON ",
        s" ${Operator("and").sql} \n            ",
        "\n"
      )

object BaseJoinClause:

  def apply(str: String)(using
      LogographConfig
  ): (Table, String, List[SQLExpression]) => BaseJoinClause =
    str match
      case "InnerJoin" => InnerJoinClause.apply _
      case "RightJoin" => RightJoinClause.apply _
      case "LeftJoin"  => LeftJoinClause.apply _

case class InnerJoinClause(
    table: Table,
    tableAlias: String,
    exprs: List[SQLExpression]
)(using LogographConfig)
    extends BaseJoinClause("INNER JOIN")

case class LeftJoinClause(
    table: Table,
    tableAlias: String,
    exprs: List[SQLExpression]
)(using LogographConfig)
    extends BaseJoinClause("LEFT JOIN ")

case class RightJoinClause(
    table: Table,
    tableAlias: String,
    exprs: List[SQLExpression]
)(using LogographConfig)
    extends BaseJoinClause("RIGHT JOIN")

case class QueryClause(
    select: Option[BaseSelectClause] = None,
    from: Option[FromClause] = None,
    joins: List[BaseJoinClause] = List.empty,
    wher: Option[WhereClause] = None,
    orderBy: Option[OrderByClause] = None
)(using LogographConfig)
    extends SQLExpressionClause:

  lazy val exprs =
    select.foldMap(_.exprs) ++ from.foldMap(_.exprs) ++ joins.foldMap(
      _.exprs
    ) ++
      wher.foldMap(_.exprs) ++ orderBy.foldMap(_.exprs)

  private val aggFields =
    (List(select, wher, orderBy).flatMap(getAggFields)) ++ getAggFields(joins)
  private val nonAggFields =
    (List(select, wher, orderBy).flatMap(getNonAggFields)) ++ getNonAggFields(
      joins
    )
  private val allFields =
    (List(select, wher, orderBy).flatMap(getAllFields)) ++ getAllFields(joins)

  private val groupBy =

    val nonAggFields = allFields.distinct.diff(aggFields.distinct)

    if aggFields.nonEmpty && nonAggFields.nonEmpty then
      Some(GroupByClause(nonAggFields))
    else None

  private val where =

    val nonAggPreds =
      wher.fold(List.empty[SQLExpression])(_.exprs.filter(_.aggFields.isEmpty))

    wher.map(_.copy(exprs = nonAggPreds))

  private val having =

    val aggPreds =
      wher.fold(List.empty[SQLExpression])(_.exprs.filter(_.aggFields.nonEmpty))

    if aggPreds.nonEmpty then Some(HavingClause(aggPreds))
    else None

  lazy val sql =
    select.foldMap(_.sql) +
      from.foldMap(_.sql) +
      joins.map(_.sql).mkString("") +
      where.foldMap(_.sql) +
      groupBy.foldMap(_.sql) +
      having.foldMap(_.sql) +
      orderBy.foldMap(_.sql)

  override lazy val validate =

    val overlappedFields = aggFields.intersect(nonAggFields)

    val unknownOrderByExprs =
      orderBy
        .fold(List.empty[SQLExpression])(_.exprs)
        .diff(select.fold(List.empty[SQLExpression])(_.exprs))

    val isSelectDistinct =
      select.fold(false)(x =>
        x.isInstanceOf[SelectDistinctClause] ||
          x.isInstanceOf[SelectDistinctAllClause]
      )

    if select.isEmpty then Some(NoSelectClause())
    else if overlappedFields.nonEmpty then
      Some(OverlappedAggregatedFields(overlappedFields))
    else if (groupBy.isDefined || isSelectDistinct) && unknownOrderByExprs.nonEmpty
    then Some(UnknownOrderByExprs(unknownOrderByExprs))
    else None

end QueryClause

case class SetClause(setMap: Map[Field, SQLExpression])(using LogographConfig)
    extends SQLExpressionClause:

  lazy val exprs = (setMap.keys ++ setMap.values).toList

  lazy val sql =
    setMap
      .map { case (field, expr) => s"${field.sql} = ${expr.sql}" }
      .mkString("SET         ", ",\n            ", "")

case class UpdateClause(
    table: Table,
    setClause: SetClause,
    whereClause: Option[WhereClause]
)(using LogographConfig)
    extends SQLExpressionClause:

  lazy val exprs = whereClause.foldMap(_.exprs) ++ setClause.exprs

  lazy val sql =

    val setClauseSql = removeAliases(setClause.sql)
    val whereClauseSql = whereClause.foldMap(x => removeAliases(x.sql))

    s"""|UPDATE      ${table.sql}
        |$setClauseSql
        |$whereClauseSql""".stripMargin

case class DeleteClause(table: Table, whereClause: Option[WhereClause])(using
    LogographConfig
) extends SQLExpressionClause:

  lazy val exprs = whereClause.foldMap(_.exprs)

  lazy val sql =

    val whereClauseSql = whereClause.foldMap(x => removeAliases(x.sql))

    s"""|DELETE FROM ${table.sql}
        |$whereClauseSql""".stripMargin

object SQLExpressionClause:

  def findParameters(model: Any): Map[String, Any] =

    model match
      case ident: Identity   => ident.parameter
      case prod: Product     =>
        prod.productIterator.map(findParameters).fold(Map.empty)(_ ++ _)
      case iter: Iterable[?] => iter.map(findParameters).fold(Map.empty)(_ ++ _)
      case _                 => Map.empty

  def getAllFields[F[_]: Foldable](clause: F[SQLExpressionClause]) =
    clause.foldMap(_.exprs.flatMap(_.fields))

  def getAggFields[F[_]: Foldable](clause: F[SQLExpressionClause]) =
    clause.foldMap(_.exprs.flatMap(_.aggFields))

  def getNonAggFields[F[_]: Foldable](clause: F[SQLExpressionClause]) =
    clause.foldMap(_.exprs.flatMap(_.nonAggFields))

  def getValidationErrors(clause: SQLExpressionClause) =
    clause.validate ++ List(clause).flatMap(_.validate)

  def removeAliases(exprClauseSql: String) =
    exprClauseSql.replaceAll("[a-zA-Z]\\.\\[", "[")
