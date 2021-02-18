package orm

trait SQLClause extends PrettyPrint {
  val sql: String
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


object Predicate {

  def adaptSql (exp: Expression) = exp match {
      case op: Operation => op.sql
      case _ => s"${exp.sql} = 1"
  }
}

case class LiteralVal (value: String) extends Expression {

  val sql = value
}

case class Field (tableAlias: String, column: String) extends Expression {

  val sql = s"$tableAlias.[$column]"
}

case class Identity (name: String, tree: Option[Any] = None) extends Expression {

  val sql = s"@${name.replace("this.","")}"

  val parameters = tree.fold (Map.empty[String, Any]) (x => Map(sql -> x))
}


sealed abstract class OpType
case object Infix  extends OpType
case object Prefix  extends OpType
case object Postfix  extends OpType

object Operation {

  val aggOps = List("sum", "product", "stringAgg")

  val infixOps = List("-", "+", "*", "/", "===", "<>", "<", ">", "LIKE")

  val postfixOps = List("DESC", "ASC")

  val opsConversion = Map("unary_!" -> "NOT", "&&" -> "AND", "||" -> "OR")
}

case class Operation (operator: String, operands: List[Expression]) extends Expression {

  import Operation._

  val opType = if       (infixOps.contains(operator))        Infix
               else if  (postfixOps.contains(operator))      Postfix
               else                                          Prefix

  val newOperator =  opsConversion.getOrElse(operator, operator)

  val sql = opType match {

      case Infix ⇒ operands.map(_.sql)
                              .mkString(s" $newOperator ")

      case Postfix ⇒ operands.head.sql + " " + newOperator

      case Prefix ⇒ {

        val operandsStr = operands.map(_.sql)
                        .mkString(", ")

        s"$newOperator ($operandsStr)"
      }
  }
}
