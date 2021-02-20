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

  val aggOps = List("count", "sum", "avg", "max", "min", "stringAgg")

  val infixOps = List("-", "+", "*", "/", "==", "!=", "<", ">", "like")

  val postfixOps = List("desc", "asc")

  val opsConversion = Map("unary_!" -> "not", "&&" -> "and", "||" -> "or",
                          "==" -> "=", "!=" -> "<>", "isNull" -> "isnull")

}

case class Operation (operator: String, operands: List[Expression]) extends Expression {

  import Operation._

  val opType = if       (infixOps.contains(operator))        Infix
               else if  (postfixOps.contains(operator))      Postfix
               else                                          Prefix

  private val newOperator = toUnderscoreCase(opsConversion.getOrElse(operator, operator))
                                         .toUpperCase


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

  private def toUnderscoreCase (st: String) =
    splitWhere(st, _.isUpper).mkString("_")

  private def splitWhere (st: String, fn: Char => Boolean) = {

    import scala.collection.mutable.ListBuffer
    val indexes = ListBuffer(0)
    var s = st

    while(s.indexWhere(fn) >= 0) {
      indexes += s.indexWhere(fn)
      s = s.substring(indexes.last + 1)
    }

    indexes += st.size

    indexes.sliding(2)
           .map(x => if (x.size == 2) st.slice(x(0), x(1)) else x(0))
           .toList
  }
}
