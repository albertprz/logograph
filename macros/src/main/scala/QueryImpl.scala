package ast

import orm._
import scala.reflect.macros.blackbox
import scala.reflect.api.Liftables
import scala.reflect.api.StandardLiftables


class QueryImpl (val c: blackbox.Context) {

  import c.universe._

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
    q"FromClause(${from.aliases}, ${from.tableNames})"
  }

  implicit val liftQuery = Liftable[QueryClause] { qry =>
    q"""QueryClause(${qry.select}, ${qry.where},
                    ${qry.orderBy}, ${qry.from})"""
  }


  def compile [T: WeakTypeTag, R: WeakTypeTag] (queryFnTree: Tree) = {

    val ops = new TreeOps [c.type] (c)

    val selectClause = ops.getSelectClause(queryFnTree)
    val whereClause = ops.getWhereClause(queryFnTree)
    val orderByClause = ops.getOrderByClause(queryFnTree)
    val fromClause = ops.getFromClause(queryFnTree, weakTypeOf[T].toString)

    val queryClause = QueryClause(selectClause, whereClause, orderByClause, fromClause)


    c.Expr[FinalQuery[T, R]](q"""FinalQuery(queryClause = Some($queryClause))""")
  }
}
