package com.albertoperez1994.scalaql.core

import com.albertoperez1994.scalaql.utils.StringUtils._
import com.albertoperez1994.scalaql.config.ScalaQLConfig
import com.albertoperez1994.scalaql.{SelectDistinct, SelectDistinctAll}

trait ExpressionClause extends SQLClause {
  val exprs: List[Expression]
}

sealed trait BaseSelectClause extends ExpressionClause

case class SelectClause (exprs: List[Expression]) extends BaseSelectClause {

  val validate = {}

  def sql() (implicit cfg: ScalaQLConfig) =
    exprs
    .map(_.sql)
    .mkString("SELECT      ", ", ", "\n")
}

case class SelectAllClause (tableAlias: String) extends BaseSelectClause {

  val exprs = List.empty

  val validate = {}

  def sql() (implicit cfg: ScalaQLConfig) =
    s"SELECT      $tableAlias.*\n"
}

case class SelectDistinctClause (exprs: List[Expression]) extends BaseSelectClause {

  val validate = {}

  def sql() (implicit cfg: ScalaQLConfig) =
    exprs
      .map(_.sql)
      .mkString("SELECT      DISTINCT ", ", ", "\n")
}

case class SelectDistinctAllClause (tableAlias: String) extends BaseSelectClause {

  val exprs = List.empty

  val validate = {}

  def sql() (implicit cfg: ScalaQLConfig) =
    s"SELECT      DISTINCT $tableAlias.*\n"
}

case class FromClause (tableAliases: Map[String, String]) extends SQLClause {

  val validate = {}

  def sql() (implicit cfg: ScalaQLConfig) =
    tableAliases
      .map { case (tableAlias, tableName) =>
          s"[${Table(tableName).sql}] AS $tableAlias" }
      .mkString("FROM        ", ", ", "\n")
}

case class WhereClause (exprs: List[Expression]) extends ExpressionClause {

  val validate = {}

  def sql() (implicit cfg: ScalaQLConfig) =
    exprs
      .map(Predicate.adaptSql)
      .map(str => if (exprs.size > 1) s"($str)" else str)
      .mkString("WHERE       ", " AND \n            ", "\n")
}

case class GroupByClause (fields: List[Field]) extends SQLClause {

  val validate = {}

  def sql() (implicit cfg: ScalaQLConfig) =
    fields
      .map(_.sql)
      .mkString("GROUP BY    ", ", ", "\n")
}

case class HavingClause (exprs: List[Expression]) extends ExpressionClause {

  val validate = {}

  def sql() (implicit cfg: ScalaQLConfig) =
    exprs
      .map(Predicate.adaptSql)
      .map(str => if (exprs.size > 1) s"($str)" else str)
      .mkString("HAVING      ", " AND \n            ", "\n")
}

case class OrderByClause (exprs: List[Expression]) extends ExpressionClause {

  val validate = {

    for (expr <- exprs)
      expr match {
        case fld: Field => {}
        case op: Operation => {}
        case _ => throw new Exception(s"Only fields and operations are allowed in an Order By Clause")
      }
  }

  def sql() (implicit cfg: ScalaQLConfig) =
    exprs
      .map(_.sql)
      .mkString("ORDER BY    ", ", ", "\n")
}

sealed abstract class BaseJoinClause extends ExpressionClause {

  val tableName: String
  val tableAlias: String
  val exprs: List[Expression]

  val joinType = this match {
    case inner: InnerJoinClause => "INNER JOIN"
    case left: LeftJoinClause   => "LEFT JOIN "
    case right: RightJoinClause => "RIGHT JOIN"
  }

  val validate = {}

  def sql() (implicit cfg: ScalaQLConfig) =
    exprs
      .map(Predicate.adaptSql)
      .map(str => if (exprs.size > 1) s"($str)" else str)
      .mkString(s"$joinType  [$tableName] AS $tableAlias ON ", " AND \n            ", "\n")
}

case class InnerJoinClause (tableName: String, tableAlias: String, exprs: List[Expression])
    extends BaseJoinClause

case class LeftJoinClause (tableName: String, tableAlias: String, exprs: List[Expression])
    extends BaseJoinClause

case class RightJoinClause (tableName: String, tableAlias: String, exprs: List[Expression])
    extends BaseJoinClause

case class QueryClause (select: Option[BaseSelectClause] = None, from: Option[FromClause] = None,
                        joins: List[BaseJoinClause] = List.empty, wher: Option[WhereClause] = None,
                        orderBy: Option[OrderByClause] = None)
                        extends SQLClause {

  import ExpressionClause._

  private val aggFields = (List(select, wher, orderBy) flatMap getAggFields) ++ getAggFields(joins)
  private val nonAggFields = (List(select, wher, orderBy) flatMap getNonAggFields) ++ getNonAggFields(joins)
  private val allFields = (List(select, wher, orderBy) flatMap getAllFields) ++ getAllFields(joins)

  private val groupBy = {

    val nonAggFields = allFields.distinct diff aggFields.distinct

     if (aggFields.nonEmpty && nonAggFields.nonEmpty) Some(GroupByClause(nonAggFields))
     else None
  }

  private val where = {

    val nonAggPreds = wher.fold(List.empty[Expression])(_.exprs.filter(_.aggFields.isEmpty))

     wher map (_.copy(exprs = nonAggPreds))
  }

  private val having = {

    val aggPreds = wher.fold(List.empty[Expression])(_.exprs.filter(_.aggFields.nonEmpty))

     if (aggPreds.nonEmpty) Some(HavingClause(aggPreds))
     else None
  }

  val validate = {

    if (select.isEmpty) {
      throw new Exception("No Select Clause was found for the current query")
    }

    val overlappedFields = aggFields intersect nonAggFields

    if (overlappedFields.nonEmpty) {
      throw new Exception("""|\nThere are some fields which are being used both in aggregate functions and
                             | as regular columns:\n${pprint(overlappedFields)}
                             |Please only use these fields within an aggregation function \n""".stripMargin)
    }

    val orderByExprs = orderBy.fold (List.empty[Expression]) (_.exprs) diff
                       select.fold  (List.empty[Expression]) (_.exprs)

      if ((groupBy.isDefined || select.fold(false)(x => x.isInstanceOf[SelectDistinct[_]] ||
                                                        x.isInstanceOf[SelectDistinctAll[_]])) &&
         orderByExprs.nonEmpty) {
      throw new Exception(s"""|\nThere are some expressions used in the Order By Clause, which were not
                              |included in the Select Clause:\n${pprint(orderByExprs)}
                              |Please only use expressions already included in the Select clause
                              |for the Order By clause if the query is using
                              | a Group By or Distinct clause \n""".stripMargin)
    }
  }

  def sql() (implicit cfg: ScalaQLConfig = ScalaQLConfig()) = {

      select.fold("")(_.sql)           +
      from.fold("")(_.sql)             +
      joins.map(_.sql).mkString("")    +
      where.fold("")(_.sql)            +
      groupBy.fold("")(_.sql)          +
      having.fold("")(_.sql)           +
      orderBy.fold("")(_.sql)
  }
}

case class SetClause (setMap: Map[Field, Expression]) extends ExpressionClause {

  val exprs = (setMap.keys ++ setMap.values).toList

  val validate = {}

  def sql() (implicit cfg: ScalaQLConfig) =
    setMap.map { case (key, value) => s"[${Column(key.column).sql}] = ${value.sql}" }
          .mkString("SET         ", ",\n            ", "")
}

case class UpdateClause (tableName: String, setClause: SetClause,
                         whereClause: Option[WhereClause]) extends SQLClause {

  val validate = {}

  def sql() (implicit cfg: ScalaQLConfig = ScalaQLConfig()) = {

    val setClauseSql = setClause.sql
    val whereClauseSql = whereClause.fold ("") (ExpressionClause.removeAliases)

   s"""|UPDATE      [${Table(tableName).sql}]
       |$setClauseSql
       |$whereClauseSql""".stripMargin
  }
}

case class DeleteClause (tableName: String, whereClause: Option[WhereClause]) extends SQLClause {

  val validate = {}

  def sql() (implicit cfg: ScalaQLConfig = ScalaQLConfig()) = {

   val whereClauseSql = whereClause.fold ("") (ExpressionClause.removeAliases)

    s"""|DELETE FROM [${Table(tableName).sql}]
        |$whereClauseSql""".stripMargin
  }
}


object ExpressionClause {

  def findParameters(model: Any): Map[String, Any] =

     model match {
      case ident: Identity => ident.parameter
      case prod: Product => prod.productIterator.map(findParameters).fold(Map.empty)(_++_)
      case iter: Iterable[Any] => iter.map(findParameters).fold(Map.empty)(_++_)
      case _ => Map.empty
    }

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

  def removeAliases (exprClause: ExpressionClause) (implicit cfg: ScalaQLConfig) =
    exprClause.sql.replaceAll("[a-zA-Z]\\.\\[", "[")
}
