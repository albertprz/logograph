package com.albertoperez1994.scalaql.macros

import scala.reflect.macros.blackbox

import com.albertoperez1994.scalaql._
import com.albertoperez1994.scalaql.core._

class QueryImpl(val c: blackbox.Context) {

  import c.universe._

  def selectAll[T <: DbDataSet] () (implicit tag: WeakTypeTag[T]) =
    buildQueryAll[T] ()

  def select[T, R <: DbDataSet] (query: Tree) (implicit tag1: WeakTypeTag[T], tag2: WeakTypeTag[R]) =
    buildQuery[T, R] (query)

  def selectDebug[T, R <: DbDataSet] (query: Tree) (implicit tag1: WeakTypeTag[T], tag2: WeakTypeTag[R]) =
    buildQuery[T, R] (query, debug = true)

  def updateAll[T <: DbTable] (setMap: Tree) (implicit tag: WeakTypeTag[T]) =
    buildUpdate[T] (setMap)

  def update[T <: DbTable] (setMap: Tree) (implicit tag: WeakTypeTag[T]) =
    buildUpdate[T] (setMap)

  def updateDebug[T <: DbTable] (setMap: Tree) (implicit tag: WeakTypeTag[T]) =
    buildUpdate[T] (setMap, debug = true)

  def deleteAll[T <: DbTable] (implicit tag: WeakTypeTag[T])  =
    buildDelete[T] ()

  def delete[T <: DbTable] (where: Tree)  (implicit tag: WeakTypeTag[T])  =
    buildDelete[T] (Some(where))

  def deleteDebug[T <: DbTable] (where: Tree)  (implicit tag: WeakTypeTag[T])  =
    buildDelete[T] (Some(where), debug = true)

  private val extractor = new QueryExtractor[c.type](c)

  implicit val liftExpression: Liftable[Expression] = Liftable[Expression] {exp =>

    exp match {
      case fld: Field => q"Field(${fld.tableAlias}, ${fld.column})"
      case op: Operation => q"Operation(${op.operator}, ${op.operands})"
      case ltr: LiteralVal => q"LiteralVal(${ltr.value})"
      case id: Identity => q"Identity(${id.name}, null)"
    }
  }

  implicit val liftBaseSelect: Liftable[BaseSelectClause] = Liftable[BaseSelectClause] {select =>

    select match {
      case select: SelectClause => q"SelectClause(${select.exprs})"
      case select: SelectAllClause => q"SelectAllClause(${select.tableAlias})"
      case select: SelectDistinctClause => q"SelectDistinctClause(${select.exprs})"
      case select: SelectDistinctAllClause => q"SelectDistinctAllClause(${select.tableAlias})"
    }
  }

  implicit val liftBaseJoin: Liftable[BaseJoinClause] = Liftable[BaseJoinClause] {join =>

    join match {
      case join: InnerJoinClause => q"InnerJoinClause(${join.tableName}, ${join.tableAlias}, ${join.exprs})"
      case join: LeftJoinClause => q"LeftJoinClause(${join.tableName}, ${join.tableAlias}, ${join.exprs})"
      case join: RightJoinClause => q"RightJoinClause(${join.tableName}, ${join.tableAlias}, ${join.exprs})"
    }
  }

  implicit val liftField = Liftable[Field] { fld =>
    q"Field(${fld.tableAlias}, ${fld.column})"
  }

  implicit val liftOperation = Liftable[Operation] { op =>
    q"Operation(${op.operator}, ${op.operands})"
  }

  implicit val liftSelect = Liftable[SelectClause] { select =>
    q"SelectClause(${select.exprs})"
  }

  implicit val liftSelectAll = Liftable[SelectAllClause] { select =>
    q"SelectAllClause(${select.tableAlias})"
  }

  implicit val liftSelectDistinct = Liftable[SelectDistinctClause] { select =>
    q"SelectDistinctClause(${select.exprs})"
  }

  implicit val liftSelectDistinctAll = Liftable[SelectDistinctAllClause] { select =>
    q"SelectDistinctAllClause(${select.tableAlias})"
  }

  implicit val liftWhere = Liftable[WhereClause] { where =>
    q"WhereClause(${where.exprs})"
  }

  implicit val liftGroupBy = Liftable[GroupByClause] { groupBy =>
    q"GroupByClause(${groupBy.fields})"
  }

  implicit val liftOrderBy = Liftable[OrderByClause] { orderBy =>
    q"OrderByClause(${orderBy.exprs})"
  }

  implicit val liftFrom = Liftable[FromClause] { from =>
    q"FromClause(${from.tableAliases})"
  }

  implicit val liftQuery = Liftable[QueryClause] { qry =>
    q"""QueryClause(${qry.select}, ${qry.from}, ${qry.joins},
                    ${qry.wher}, ${qry.orderBy})"""
  }

  implicit  val liftSet = Liftable[SetClause] { set =>
    q"""SetClause(${set.setMap})"""
  }

  implicit  val liftUpdate = Liftable[UpdateClause] { update =>
    q"""UpdateClause(${update.tableName}, ${update.setClause},
                     ${update.whereClause})"""
  }

  implicit val liftDelete = Liftable[DeleteClause] { delete =>
    q"""DeleteClause(${delete.tableName}, ${delete.whereClause})"""
  }


  private def buildQuery[T, R <: DbDataSet] (queryTree: Tree, debug: Boolean = false)
                                            (implicit tag1: WeakTypeTag[T], tag2: WeakTypeTag[R]) = {

    val (clause, params) = extractor.getQueryClause(queryTree, weakTypeOf[T].toString)

    if (debug) {
      throw new Exception(s""" |  Debugging query: \n\n\n${clause.sql}\n\n
                               |Query Tree: \n ${clause}\n\n\n""".stripMargin)
    }

    c.Expr[SelectStatement[R]](q"""SelectStatement(clause = $clause,
                                                   params = ${params.asInstanceOf[Map[String, Tree]]})""")
  }

  private def buildQueryAll[T <: DbDataSet] () (implicit tag: WeakTypeTag[T]) = {

    val clause = extractor.getQueryClause(weakTypeOf[T].toString)

    c.Expr[SelectStatement[T]](q"""SelectStatement(clause = $clause,
                                                   params = Map.empty[String, Any])""")
  }

  private def buildUpdate[T <: DbTable] (updateTree: Tree, debug: Boolean = false)
                         (implicit tag: WeakTypeTag[T]) = {

     val (clause, params) = extractor.getUpdateClause(updateTree, weakTypeOf[T].toString)

    if (debug) {
      throw new Exception(s""" |  Debugging update: \n\n\n${clause.sql}\n\n
                               |Update Tree: \n ${clause}\n\n\n""".stripMargin)
    }

    c.Expr[UpdateStatement[T]](q"""UpdateStatement(clause = $clause,
                                                   params = ${params.asInstanceOf[Map[String, Tree]]})""")
  }

  private def buildDelete[T <: DbTable] (whereTree: Option[Tree] = None, debug: Boolean = false)
                         (implicit tag: WeakTypeTag[T]) = {

     val (clause, params) = extractor.getDeleteClause(whereTree, weakTypeOf[T].toString)

    if (debug) {
      throw new Exception(s""" |  Debugging delete: \n\n\n${clause.sql}\n\n
                               |Delete Tree: \n ${clause}\n\n\n""".stripMargin)
    }

    c.Expr[DeleteStatement[T]](q"""DeleteStatement(clause = $clause,
                                                   params = ${params.asInstanceOf[Map[String, Tree]]})""")
  }
}
