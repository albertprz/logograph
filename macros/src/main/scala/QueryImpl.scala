package ast

import orm._
import scala.reflect.macros.blackbox
import scala.reflect.api.Liftables
import scala.reflect.api.StandardLiftables

class QueryImpl(val c: blackbox.Context) {

  import c.universe._

  def select[T: WeakTypeTag, R: WeakTypeTag] (queryFnTree: Tree) =
    buildFullQuery[T, R] (queryFnTree)

  def selectDebug[T: WeakTypeTag, R: WeakTypeTag] (queryFnTree: Tree) =
    buildFullQuery[T, R] (queryFnTree, debug = true)


  val ops = new TreeOps[c.type](c)

 implicit val liftExpression: Liftable[Expression] = Liftable[Expression] {exp =>
    liftExpression

    exp match {
      case fld: Field => q"Field(${fld.tableAlias}, ${fld.column})"
      case op: Operation => q"Operation(${op.operator}, ${op.operands})"
      case ltr: LiteralVal => q"LiteralVal(${ltr.value})"
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

  implicit val liftWhere = Liftable[WhereClause] { where =>
    q"WhereClause(${where.preds})"
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

  implicit val liftBaseJoin = Liftable[BaseJoinClause] { join =>

    join match {
      case inner: InnerJoinClause =>
        q"InnerJoinClause(${join.tableName}, ${join.tableAlias}, ${join.preds})"
      case left:  LeftJoinClause =>
        q"LeftJoinClause(${join.tableName}, ${join.tableAlias}, ${join.preds})"
      case right: RightJoinClause =>
        q"RightJoinClause(${join.tableName}, ${join.tableAlias}, ${join.preds})"
    }
  }

  implicit val liftQueryClause = Liftable[QueryClause] { qry =>
    q"""QueryClause(${qry.select}, ${qry.where},
                    ${qry.orderBy}, ${qry.from}, ${qry.joins})"""
  }

  def buildFullQuery[T: WeakTypeTag, R: WeakTypeTag] (queryFnTree: Tree, debug: Boolean = false) = {

    val tableAliases = ops.getTableAliases(queryFnTree, weakTypeOf[T].toString)

    val selectClause = ops.getSelectClause(queryFnTree)
    val whereClause = ops.getWhereClause(queryFnTree)
    val orderByClause = ops.getOrderByClause(queryFnTree)
    val joinClauses = ops.getJoinClauses(queryFnTree, tableAliases)
    val fromClause = ops.getFromClause(queryFnTree, tableAliases, joinClauses)

    val queryClause = QueryClause (selectClause, whereClause,
                                   orderByClause, fromClause, joinClauses)

    if (debug) {
      throw new Exception(s"  Debugging query: \n\n\n${queryClause.sql}\n")
    }

    c.Expr[FullQuery[T, R]](q"""FullQuery(queryClause = Some($queryClause))""")
    // c.Expr[FullQuery[T, R]](q"""FullQuery()""")
  }
}
