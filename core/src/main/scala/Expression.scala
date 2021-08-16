package com.albertoperez1994.scalaql.core

import com.albertoperez1994.scalaql.utils.StringUtils._
import com.albertoperez1994.scalaql.utils.PrettyPrintTree
import com.albertoperez1994.scalaql.config.ScalaQLConfig

trait SQLClause extends PrettyPrintTree {

  val validate: Unit
  def sql() (implicit cfg: ScalaQLConfig): String
}

sealed trait Expression extends SQLClause {

  val fields = Expression.findFields(this)
  val aggFields = Expression.findAggFields(this)
  val nonAggFields = fields diff aggFields
}


case class LiteralVal (value: String) extends Expression {

  val validate = {}
  def sql() (implicit cfg: ScalaQLConfig) = value
}

case class Field (tableAlias: String, column: String) extends Expression {

  val validate = {}
  def sql() (implicit cfg: ScalaQLConfig) =
     s"$tableAlias." + (if (column != "*") s"[${Column(column).sql}]" else column)
}

case class Identity (name: String, tree: Any) extends Expression {

  val validate = {}
  def sql() (implicit cfg: ScalaQLConfig) = "?"

  private val paramName = name.replace("this.", "")
  val parameter = Map(s"@$paramName" -> tree)
}


sealed trait OpType
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

  def sql() (implicit cfg: ScalaQLConfig) = {

    val convOperator = Operator(opsConversion.getOrElse(operator, operator)).sql

    opType match {
      case Infix ⇒ operands.map(_.sql).mkString(s" $convOperator ")
      case Postfix ⇒ s"${operands.head.sql} $convOperator"
      case Prefix ⇒  s"$convOperator (${operands.map(_.sql).mkString(", ")})"
    }
  }
}


case class Operator(str: String) extends SQLClause {

  val validate = {}
  def sql() (implicit cfg: ScalaQLConfig) = convertCase(cfg.operatorConverter, str)
}

case class Column(str: String) extends SQLClause {

  val validate = {}
  def sql() (implicit cfg: ScalaQLConfig) = convertCase(cfg.columnConverter, str)
}

case class Table(str: String) extends SQLClause {

  val validate = {}
  def sql() (implicit cfg: ScalaQLConfig) = convertCase(cfg.tableConverter, str)
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

  def adaptSql (exp: Expression) (implicit cfg: ScalaQLConfig) = exp match {
      case op: Operation => op.sql
      case _ => s"${exp.sql} = 1"
  }
}
