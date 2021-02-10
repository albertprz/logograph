
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

abstract class OpType
case class Infix () extends OpType
case class Prefix () extends OpType
case class Postfix () extends OpType

object Operation {

  val aggOps = List("sum", "product", "stringAgg")

  val infixOps = List("-", "+", "*", "/", "==", "!=", "like")

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


abstract class Expression extends SQLClause {

  import Operation._

  private def findAggFields (isAgg: Boolean = false): List[Field] =
    this match {
      case ltr: LiteralVal => List.empty
      case fld: Field => if (isAgg) List(fld) else List.empty
      case op: Operation => op.operands
                              .flatMap(_.findAggFields(aggOps.contains(op.operator)))
    }

  val fields: List[Field] =
    this match {
      case ltr: LiteralVal => List.empty
      case fld: Field => List(fld)
      case op: Operation => op.operands
                              .flatMap(_.fields)
    }

  val aggFields = findAggFields()

  val nonAggFields = fields diff aggFields
}


case class SelectClause (exprs: List[Expression]) extends SQLClause {

  val sql = exprs
            .map(_.sql)
            .mkString("SELECT     ", ", ", "\n")
}

case class WhereClause (preds: List[Operation]) extends SQLClause {

  val sql = preds
            .map(x => s"(${x.sql})")
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

case class FromClause (aliases : List[String], tableNames: List[String]) extends SQLClause {

  val sql = (aliases zip tableNames)
              .map { case (alias, tableName) => s"[$tableName] AS $alias" }
              .mkString("FROM       ", ", ", "\n")
}


case class QueryClause (select: Option[SelectClause] = None, where: Option[WhereClause] = None,
                        orderBy: Option[OrderByClause] = None, from: Option[FromClause] = None)
                        extends SQLClause {


  val sql = {

    val nonAggFields = select.fold (List.empty[Field]) (_.exprs.flatMap(_.nonAggFields))
    val aggFields = select.fold (List.empty[Field]) (_.exprs.flatMap(_.aggFields))

    val groupBy = if (!aggFields.isEmpty && !nonAggFields.isEmpty) Some(GroupByClause(nonAggFields))
                  else None

      select.fold("")(_.sql)  +
      from.fold("")(_.sql)    +
      where.fold("")(_.sql)   +
      groupBy.fold("")(_.sql) +
      orderBy.fold("")(_.sql)
  }
}
