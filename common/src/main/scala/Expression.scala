package orm

import utils.StringUtils._

trait SQLClause extends PrettyPrintTree {

  val validate: Unit
  val sql: String
}

sealed abstract class Expression extends SQLClause {

  val fields = Expression.findFields(this)
  val aggFields = Expression.findAggFields(this)
  val nonAggFields = fields diff aggFields
}


case class LiteralVal (value: String) extends Expression {

  val validate = {}
  val sql = value
}

case class Field (tableAlias: String, column: String) extends Expression {

  val validate = {}
  val sql = s"$tableAlias." + (if (column != "*") s"[$column]" else column)
}

case class Identity (name: String, tree: Any) extends Expression {

  val validate = {}
  val sql = "?"

  private val paramName = name.replace("this.", "")
  val parameter = Map(s"@$paramName" -> tree)
}


sealed abstract class OpType
case object Infix  extends OpType
case object Prefix  extends OpType
case object Postfix  extends OpType


case class Operation (operator: String, operands: List[Expression]) extends Expression {

  import QueryOps._

  val opType = if       (infixOps.contains(operator))        Infix
               else if  (postfixOps.contains(operator))      Postfix
               else if  (prefixOps.contains(operator))       Prefix


  val validate = {

    if (!allOps.contains(operator))
        throw new Exception(s"Operator $operator is not valid in a query \n")
  }

  val sql = {

    val newOperator = CamelCase(opsConversion.getOrElse(operator, operator))
                        .toCase[SnakeCase]

    opType match {
      case Infix ⇒ operands.map(_.sql).mkString(s" $newOperator ")
      case Postfix ⇒ s"${operands.head.sql} $newOperator"
      case Prefix ⇒  s"$newOperator (${operands.map(_.sql).mkString(", ")})"
    }
  }
}


private object Expression {

  private def findAggFields (expr: Expression, isAgg: Boolean = false): List[Field] =
    expr match {
      case fld: Field => if (isAgg) List(fld) else List.empty
      case op: Operation => op.operands
                              .flatMap(findAggFields(_, isAgg || QueryOps.aggOps.contains(op.operator)))
      case _ => List.empty
    }

  def findFields (expr: Expression): List[Field] =
    expr match {
      case fld: Field => List(fld)
      case op: Operation => op.operands
                              .flatMap(findFields)
      case _ => List.empty
    }
}

private object Predicate {

  def adaptSql (exp: Expression) = exp match {
      case op: Operation => op.sql
      case _ => s"${exp.sql} = 1"
  }
}
