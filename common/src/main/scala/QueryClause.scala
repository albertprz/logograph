
package orm

trait SQLClause {
  val sql: String
}

abstract class Expression extends SQLClause {
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


case class Operation (operator: String, operands: List[Expression]) extends Expression {

  val opType = if (Seq("-", "+", "*", "/", "==", "!=", "like").contains(operator))
                 Infix()
               else if  (Seq("desc", "asc").contains(operator))
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


case class SelectClause (exprs: List[Expression]) extends SQLClause {

  val sql = exprs
            .map(_.sql)
            .mkString("SELECT     ", ", ", "\n")
}

case class WhereClause (preds: List[Operation]) extends SQLClause {

  val sql = preds
            .map(_.sql)
            .mkString("WHERE      ", " AND ", "\n")
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
                        groupBy: Option[GroupByClause] = None, orderBy: Option[OrderByClause] = None,
                        from: Option[FromClause] = None)
                        extends SQLClause {

  val sql = select.fold("")(_.sql)  +
            where.fold("")(_.sql)   +
            groupBy.fold("")(_.sql) +
            orderBy.fold("")(_.sql) +
            from.fold("")(_.sql)
}
