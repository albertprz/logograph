package orm


trait SQLClause extends PrettyPrint {

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
  val sql = s"$tableAlias.[$column]"
}

case class Identity (name: String, tree: Any) extends Expression {

  val validate = {}
  val sql = s"@${name.replace("this.","")}"
  val parameters =  Map(sql -> tree)
}


sealed abstract class OpType
case object Infix  extends OpType
case object Prefix  extends OpType
case object Postfix  extends OpType


case class Operation (operator: String, operands: List[Expression]) extends Expression {

  import Operation._

  val opType = if       (infixOps.contains(operator))        Infix
               else if  (postfixOps.contains(operator))      Postfix
               else if  (prefixOps.contains(operator))       Prefix


  val validate = {

    if (!allOps.contains(operator))
        throw new Exception(s"Operator $operator is not valid in a query \n")
  }

  val sql = {
   val newOperator = StringUtils.toUnderscoreCase(opsConversion.getOrElse(operator, operator))
                        .toUpperCase

    opType match {
      case Infix ⇒ operands.map(_.sql).mkString(s" $newOperator ")
      case Postfix ⇒ s"${operands.head.sql} $newOperator"
      case Prefix ⇒  s"$newOperator (${operands.map(_.sql).mkString(", ")})"
    }
  }
}



object Operation {

  val aggOps = List("count", "sum", "avg", "max", "min", "stringAgg")

  val infixOps = List("-", "+", "*", "/", "===", "<>", "&&", "||", "<", ">", "like", "in")

  val postfixOps = List("desc", "asc")

  val prefixOps = List("unary_!", "isNull", "count", "sum", "avg", "max", "min", "stringAgg")

  val opsConversion = Map("unary_!" -> "not", "&&" -> "and", "||" -> "or",
                           "===" -> "=", "isNull" -> "isnull")

  val allOps = infixOps ++ postfixOps ++ prefixOps
}

private object Expression {

  import Operation._

  private def findAggFields (expr: Expression, isAgg: Boolean = false): List[Field] =
    expr match {
      case fld: Field => if (isAgg) List(fld) else List.empty
      case op: Operation => op.operands
                              .flatMap(findAggFields(_, isAgg || aggOps.contains(op.operator)))
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

private object StringUtils {

  def toUnderscoreCase (st: String) =
    splitWhere(st, _.isUpper).mkString("_")

  def splitWhere (st: String, fn: Char => Boolean) = {

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
