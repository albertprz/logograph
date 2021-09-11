package com.albertoperez1994.scalaql.core

import com.albertoperez1994.scalaql.utils.StringUtils._
import com.albertoperez1994.scalaql.utils.PrettyPrintTree
import com.albertoperez1994.scalaql.config.ScalaQLConfig

trait SQLClause extends PrettyPrintTree {

  val validate: Unit
  val sql: String
}

sealed trait Expression extends SQLClause {

  val fields = Expression.findFields(this)
  val aggFields = Expression.findAggFields(this)
  val nonAggFields = fields diff aggFields
}


case class LiteralVal (value: String) (implicit cfg: ScalaQLConfig)
    extends Expression {

  val validate = {}
  val sql = value
}

case class Field (tableAlias: String, column: Column) (implicit cfg: ScalaQLConfig)
    extends Expression {

  val validate = {}
  val sql = s"$tableAlias.${column.sql}"
}

case class Identity (name: String, tree: Any) (implicit cfg: ScalaQLConfig)
    extends Expression {

  val validate = {}
  val sql = "?"

  private val paramName = name.replace("this.", "")
  val parameter = Map(s"@$paramName" -> tree)
}


sealed trait OpType
case object Infix  extends OpType
case object Prefix  extends OpType
case object Postfix  extends OpType


case class Operation (operator: String, operands: List[Expression]) (implicit cfg: ScalaQLConfig)
    extends Expression {

  import QueryOps._

  val opType = if       (infixOps.contains(operator))        Infix
               else if  (postfixOps.contains(operator))      Postfix
               else if  (prefixOps.contains(operator))       Prefix


  val validate = {

    if (!allOps.contains(operator))
        throw new Exception(s"Operator $operator is not valid in a query \n")
  }

  val sql = {

    val convOperator = Operator(opsConversion.getOrElse(operator, operator)).sql

    opType match {
      case Infix => operands.map(_.sql).mkString(s" $convOperator ")
      case Postfix => s"${operands.head.sql} $convOperator"
      case Prefix =>  s"$convOperator " + operands.map(_.sql).mkString(", ")
                                                             .wrapParens()
    }
  }
}


case class Table(tableName: String) (implicit cfg: ScalaQLConfig)
    extends SQLClause {

  val validate = {}
  val sql = tableName.convert(cfg.tableConverter)
                     .convertCase(cfg.tableCaseConverter)
                     .wrapBrackets()
}

case class Column(columnName: String, tableName: String) (implicit cfg: ScalaQLConfig)
    extends SQLClause {

  val validate = {}
  val sql = if (columnName != "*")  columnName.convert(cfg.columnConverter.getOrElse(tableName, Map.empty))
                                              .convertCase(cfg.columnCaseConverter)
                                              .wrapBrackets()
            else columnName
}

case class Operator(operatorName: String) (implicit cfg: ScalaQLConfig)
    extends SQLClause {

  val validate = {}
  val sql = operatorName.convert(cfg.operatorConverter)
                        .convertCase(cfg.operatorCaseConverter)
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
