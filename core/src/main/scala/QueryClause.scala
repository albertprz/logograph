package com.albertoperez1994.scalaql.core

import com.albertoperez1994.scalaql.utils.StringUtils.*
import com.albertoperez1994.scalaql.config.ScalaQLConfig


trait ExpressionClause extends SQLClause:
  val exprs: List[Expression]

sealed trait BaseSelectClause extends ExpressionClause

case class SelectClause (exprs: List[Expression], columnAliases: List[Column]) (using ScalaQLConfig)
    extends BaseSelectClause:

  val validate = {}

  val sql = (exprs zip columnAliases)
              .map { case (expr, alias) => s"${expr.sql} AS ${alias.sql}" }
              .mkString("SELECT      ", ", ", "\n")


case class SelectDistinctClause (exprs: List[Expression], columnAliases: List[Column]) (using ScalaQLConfig)
    extends BaseSelectClause:

  val validate = {}

  val sql = (exprs zip columnAliases)
              .map { case (expr, alias) => s"${expr.sql} AS ${alias.sql}" }
              .mkString("SELECT      DISTINCT ", ", ", "\n")


case class SelectAllClause (tableAlias: String) (using ScalaQLConfig)
    extends BaseSelectClause:

  val exprs = List.empty

  val validate = {}

  val sql = s"SELECT      $tableAlias.*\n"


case class SelectDistinctAllClause (tableAlias: String) (using ScalaQLConfig)
    extends BaseSelectClause:

  val exprs = List.empty

  val validate = {}

  val sql = s"SELECT      DISTINCT $tableAlias.*\n"


case class FromClause (tableAliasesMap: Map[String, Table]) (using ScalaQLConfig)
    extends SQLClause:

  val validate = {}

  val sql = tableAliasesMap
              .map { case (tableAlias, table) =>
                  s"${table.sql} AS $tableAlias" }
              .mkString("FROM        ", ", ", "\n")


case class WhereClause (exprs: List[Expression]) (using ScalaQLConfig)
    extends ExpressionClause:

  val validate = {}

  val sql  = exprs
              .map(Predicate.adaptSql)
              .map(str => if exprs.size > 1 then str.wrapParens() else str)
              .mkString("WHERE       ", s" ${Operator("and").sql} \n            ", "\n")


case class GroupByClause (fields: List[Field]) (using ScalaQLConfig)
    extends SQLClause:

  val validate = {}

  val sql = fields
              .map(_.sql)
              .mkString("GROUP BY    ", ", ", "\n")


case class HavingClause (exprs: List[Expression]) (using ScalaQLConfig)
    extends ExpressionClause:

  val validate = {}

  val sql  = exprs
              .map(Predicate.adaptSql)
              .map(str => if exprs.size > 1 then str.wrapParens() else str)
              .mkString("HAVING      ", s" ${Operator("and").sql} \n            ", "\n")


case class OrderByClause (exprs: List[Expression]) (using ScalaQLConfig)
    extends ExpressionClause:

  val validate =

    for expr <- exprs do
      expr match
        case fld: Field => {}
        case op: Operation => {}
        case _ => throw new Exception(s"Only fields and operations are allowed in an Order By Clause")

  val sql = exprs
              .map(_.sql)
              .mkString("ORDER BY    ", ", ", "\n")


sealed abstract class BaseJoinClause (joinType: String) (using ScalaQLConfig)
    extends ExpressionClause:

  val table: Table
  val tableAlias: String
  val exprs: List[Expression]

  val validate = {}

  val sql = exprs
              .map(Predicate.adaptSql)
              .map(str => if exprs.size > 1 then str.wrapParens() else str)
              .mkString(s"$joinType  ${table.sql} AS $tableAlias ON ",
                        s" ${Operator("and").sql} \n            ",
                        "\n")


object BaseJoinClause:

  def apply (str: String) (using ScalaQLConfig):
      (Table, String, List[Expression]) => BaseJoinClause =
    str match
      case "InnerJoin" => InnerJoinClause.apply _
      case "RightJoin" => RightJoinClause.apply _
      case "LeftJoin"  => LeftJoinClause.apply _

case class InnerJoinClause (table: Table, tableAlias: String, exprs: List[Expression])
                           (using ScalaQLConfig) extends BaseJoinClause("INNER JOIN")

case class LeftJoinClause (table: Table, tableAlias: String, exprs: List[Expression])
                          (using ScalaQLConfig) extends BaseJoinClause("LEFT JOIN ")

case class RightJoinClause (table: Table, tableAlias: String, exprs: List[Expression])
                           (using ScalaQLConfig) extends BaseJoinClause("RIGHT JOIN")


case class QueryClause (select: Option[BaseSelectClause] = None, from: Option[FromClause] = None,
                        joins: List[BaseJoinClause] = List.empty, wher: Option[WhereClause] = None,
                        orderBy: Option[OrderByClause] = None)
                       (using ScalaQLConfig)
                        extends SQLClause:

  import ExpressionClause._

  private val aggFields = (List(select, wher, orderBy) flatMap getAggFields) ++ getAggFields(joins)
  private val nonAggFields = (List(select, wher, orderBy) flatMap getNonAggFields) ++ getNonAggFields(joins)
  private val allFields = (List(select, wher, orderBy) flatMap getAllFields) ++ getAllFields(joins)

  private val groupBy =

    val nonAggFields = allFields.distinct diff aggFields.distinct

     if aggFields.nonEmpty && nonAggFields.nonEmpty then Some(GroupByClause(nonAggFields))
     else None

  private val where =

    val nonAggPreds = wher.fold(List.empty[Expression])(_.exprs.filter(_.aggFields.isEmpty))

     wher map (_.copy(exprs = nonAggPreds))

  private val having =

    val aggPreds = wher.fold(List.empty[Expression])(_.exprs.filter(_.aggFields.nonEmpty))

     if aggPreds.nonEmpty then Some(HavingClause(aggPreds))
     else None

  val validate =

    if select.isEmpty then
      throw new Exception("No Select Clause was found for the current query")

    val overlappedFields = aggFields intersect nonAggFields

    if overlappedFields.nonEmpty then
      throw new Exception("""|\nThere are some fields which are being used both in aggregate functions and
                             | as regular columns:\n${pprint(overlappedFields)}
                             |Please only use these fields within an aggregation function \n""".stripMargin)

    val orderByExprs = orderBy.fold (List.empty[Expression]) (_.exprs) diff
                       select.fold  (List.empty[Expression]) (_.exprs)

      if (groupBy.isDefined || select.fold(false)(x => x.isInstanceOf[SelectDistinctClause] ||
                                                        x.isInstanceOf[SelectDistinctAllClause])) &&
         orderByExprs.nonEmpty then
      throw new Exception(s"""|\nThere are some expressions used in the Order By Clause, which were not
                              |included in the Select Clause:\n${pprint(orderByExprs)}
                              |Please only use expressions already included in the Select clause
                              |for the Order By clause if the query is using
                              | a Group By or Distinct clause \n""".stripMargin)

  val sql = select.fold("")(_.sql)           +
            from.fold("")(_.sql)             +
            joins.map(_.sql).mkString("")    +
            where.fold("")(_.sql)            +
            groupBy.fold("")(_.sql)          +
            having.fold("")(_.sql)           +
            orderBy.fold("")(_.sql)
end QueryClause

case class SetClause (setMap: Map[Field, Expression]) (using ScalaQLConfig)
    extends ExpressionClause:

  val exprs = (setMap.keys ++ setMap.values).toList

  val validate = {}

  val sql = setMap.map { case (field, expr) => s"${field.sql} = ${expr.sql}" }
                  .mkString("SET         ", ",\n            ", "")


case class UpdateClause (table: Table, setClause: SetClause, whereClause: Option[WhereClause])
                        (using ScalaQLConfig)
    extends SQLClause:

  val validate = {}

  val sql =

    val setClauseSql =  ExpressionClause.removeAliases(setClause.sql)
    val whereClauseSql = whereClause.fold ("") (x => ExpressionClause.removeAliases(x.sql))

    s"""|UPDATE      ${table.sql}
        |$setClauseSql
        |$whereClauseSql""".stripMargin


case class DeleteClause (table: Table, whereClause: Option[WhereClause])
                        (using ScalaQLConfig)
    extends SQLClause:

  val validate = {}

  val sql  =

   val whereClauseSql = whereClause.fold ("") (x => ExpressionClause.removeAliases(x.sql))

    s"""|DELETE FROM ${table.sql}
        |$whereClauseSql""".stripMargin


object ExpressionClause:

  def findParameters(model: Any): Map[String, Any] =

     model match
      case ident: Identity => ident.parameter
      case prod: Product => prod.productIterator.map(findParameters).fold(Map.empty)(_++_)
      case iter: Iterable[Any] => iter.map(findParameters).fold(Map.empty)(_++_)
      case _ => Map.empty

  def getAllFields(clause: Option[ExpressionClause]) =
      clause.fold (List.empty[Field]) (_.exprs.flatMap(_.fields))

  def getAllFields(clause: List[ExpressionClause]) =
      clause.flatMap (_.exprs.flatMap(_.fields))

  def getAggFields(clause: Option[ExpressionClause]) =
      clause.fold (List.empty[Field]) (_.exprs.flatMap(_.aggFields))

  def getAggFields(clause: List[ExpressionClause]) =
      clause.flatMap (_.exprs.flatMap(_.aggFields))

  def getNonAggFields(clause: Option[ExpressionClause]) =
      clause.fold (List.empty[Field]) (_.exprs.flatMap(_.nonAggFields))

  def getNonAggFields(clause: List[ExpressionClause]) =
      clause.flatMap (_.exprs.flatMap(_.nonAggFields))

  def removeAliases (exprClauseSql: String) =
    exprClauseSql.replaceAll("[a-zA-Z]\\.\\[", "[")
