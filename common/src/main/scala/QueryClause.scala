
package orm

trait SQLClause {
  val sql: String
}

case class LiteralVal (value: String) extends Expression {

  val sql = value
}

case class Field (tableAlias: String, column: String) extends Expression {

  val sql = s"$tableAlias.[$column]"
}

case class Identity (names: List[String]) extends Expression {

  val sql = names.mkString("@", ".", "")
}


sealed abstract class OpType
case class Infix () extends OpType
case class Prefix () extends OpType
case class Postfix () extends OpType

object Operation {

  val aggOps = List("sum", "product", "stringAgg")

  val infixOps = List("-", "+", "*", "/", "===", "<>", "<", ">", "like")

  val postfixOps = List("desc", "asc")
}

case class Operation (operator: String, operands: List[Expression]) extends Expression {

  import Operation._

  val opType = if       (infixOps.contains(operator))
                 Infix()
               else if  (postfixOps.contains(operator))
                 Postfix()
               else
                 Prefix()

  val sql = opType match {

      case Infix() ⇒ operands.map(_.sql)
                              .mkString(s" $operator ")

      case Postfix() ⇒ operands.head.sql + " " + operator

      case Prefix() ⇒ {

        val operandsStr = operands.map(_.sql)
                        .mkString(", ")

        s"$operator ($operandsStr)"
      }
  }
}


sealed abstract class Expression extends SQLClause {

  import Operation._

  private def findAggFields (isAgg: Boolean = false): List[Field] =
    this match {
      case fld: Field => if (isAgg) List(fld) else List.empty
      case op: Operation => op.operands
                              .flatMap(_.findAggFields(aggOps.contains(op.operator)))
      case _ => List.empty
    }

  val fields: List[Field] =
    this match {
      case fld: Field => List(fld)
      case op: Operation => op.operands
                              .flatMap(_.fields)
      case _ => List.empty
    }

  val aggFields = findAggFields()

  val nonAggFields = fields diff aggFields
}


case class SelectClause (exprs: List[Expression]) extends SQLClause {

  val sql = exprs
            .map(_.sql)
            .mkString("SELECT     ", ", ", "\n")
}

case class WhereClause (preds: List[Expression]) extends SQLClause {

  val sql = preds
            .map(x => if (preds.size > 1) s"(${x.sql})" else x.sql)
            .mkString("WHERE      ", " AND \n           ", "\n")
}

case class GroupByClause (fields: List[Field]) extends SQLClause {

  val sql = fields
            .map(_.sql)
            .mkString("GROUP BY   ", ", ", "\n")
}

case class OrderByClause (exprs: List[Expression]) extends SQLClause {

  val sql = exprs
            .map(_.sql)
            .mkString("ORDER BY     ", ", ", "\n")
}

case class FromClause (tableAliases: Map[String, String]) extends SQLClause {

  val sql = tableAliases
              .map { case (tableAlias, tableName) => s"[$tableName] AS $tableAlias" }
              .mkString("FROM       ", ", ", "\n")
}

sealed abstract class BaseJoinClause extends SQLClause {

  val tableName: String
  val tableAlias: String
  val preds: List[Expression]

  val joinType = this match {
    case inner: InnerJoinClause => "INNER JOIN"
    case left: LeftJoinClause   => "LEFT JOIN "
    case right: RightJoinClause => "RIGHT JOIN"
  }

  val sql = preds
            .map(x => if (preds.size > 1) s"(${x.sql})" else x.sql)
            .mkString(s"$joinType [$tableName] AS $tableAlias ON ", " AND \n           ", "\n")
}

case class InnerJoinClause (tableName: String, tableAlias: String, preds: List[Expression])
    extends BaseJoinClause

case class LeftJoinClause (tableName: String, tableAlias: String, preds: List[Expression])
    extends BaseJoinClause

case class RightJoinClause (tableName: String, tableAlias: String, preds: List[Expression])
    extends BaseJoinClause


case class QueryClause (select: Option[SelectClause] = None, where: Option[WhereClause] = None,
                        orderBy: Option[OrderByClause] = None, from: Option[FromClause] = None,
                        joins: List[BaseJoinClause])
                        extends SQLClause {

  private val groupBy = {

    val nonAggFields = select.fold (List.empty[Field]) (_.exprs.flatMap(_.nonAggFields))
    val aggFields = select.fold (List.empty[Field]) (_.exprs.flatMap(_.aggFields))

     if (!aggFields.isEmpty && !nonAggFields.isEmpty) Some(GroupByClause(nonAggFields))
     else None
  }

  val sql = {

      select.fold("")(_.sql)      +
      from.fold("")(_.sql)        +
      joins.map(_.sql)
           .mkString("")          +
      where.fold("")(_.sql)       +
      groupBy.fold("")(_.sql)     +
      orderBy.fold("")(_.sql)
  }
}
