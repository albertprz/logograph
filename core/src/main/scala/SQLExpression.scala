package com.albertprz.logograph.core

import com.albertprz.logograph.utils.Error.*
import com.albertprz.logograph.utils.StringUtils.*
import com.albertprz.logograph.utils.PrettyPrintTree
import com.albertprz.logograph.config.LogographConfig

trait SQLClause extends PrettyPrintTree:

  def validate: Option[SQLError] = None
  def sql: String

trait SQLExpression extends SQLClause:

  lazy val fields = SQLExpression.findFields(this)
  lazy val aggFields = SQLExpression.findAggFields(this)
  lazy val nonAggFields = fields diff aggFields


case class LiteralVal (value: Either[SQLError, String]) (using LogographConfig)
    extends SQLExpression:

  override lazy val validate = value.swap.toOption

  lazy val sql = value.getOrElse("")

case class Field (tableAlias: String, column: Column) (using LogographConfig)
    extends SQLExpression:

  lazy val sql = s"$tableAlias.${column.sql}"

case class Identity (name: String, tree: Any) (using LogographConfig)
    extends SQLExpression:

  lazy val sql = "?"
  lazy val parameter = Map(s"@${name.replace("this.", "")}" -> tree)



case class Operation (operator: String, operands: List[SQLExpression]) (using LogographConfig)
    extends SQLExpression:

  enum OpType:
    case Infix, Prefix, Postfix

  import SQLOperations.*
  import OpType.*

  val opType: OpType = if      infixOps.contains(operator)   then Infix
                       else if postfixOps.contains(operator) then Postfix
                       else                                       Prefix


  override lazy val validate =

    if !allOps.contains(operator) then
        Some(InvalidOperator(operator, allOps))
    else
        None


  lazy val sql =

    val convOperator = Operator(opsConversion.getOrElse(operator, operator)).sql

    opType match
      case Infix => operands.map(_.sql).mkString(s" $convOperator ")
      case Postfix => s"${operands.head.sql} $convOperator"
      case Prefix =>  s"$convOperator " + operands.map(_.sql).mkString(", ")
                                                             .wrapParens()


case class Table(tableName: String) (using cfg: LogographConfig)
    extends SQLClause:

  lazy val sql = tableName.convert(cfg.tableConverter)
                     .convertCase(cfg.tableCaseConverter)
                     .wrapBrackets()

case class Column(columnName: String, tableName: String) (using cfg: LogographConfig)
    extends SQLClause:

  lazy val sql = if columnName != "*"
    then columnName
          .convert(cfg.columnConverter.map(_.getOrElse(tableName, Map.empty)))
          .convertCase(cfg.columnCaseConverter)
          .wrapBrackets()
    else columnName

case class Operator(operatorName: String) (using cfg: LogographConfig)
    extends SQLClause:

  lazy val sql = operatorName.convert(cfg.operatorConverter)
                             .convertCase(cfg.operatorCaseConverter)


private object SQLExpression:

  def findAggFields (expr: SQLExpression, isAgg: Boolean = false): List[Field] =
    expr match
      case fld: Field    => if isAgg then List(fld) else List.empty
      case op: Operation => op.operands
        .flatMap(findAggFields(_, isAgg || SQLOperations.aggOps.contains(op.operator)))
      case _ => List.empty

  def findFields (expr: SQLExpression): List[Field] =
    expr match
      case fld: Field    => List(fld)
      case op: Operation => op.operands.flatMap(findFields)
      case _ => List.empty

private object Predicate:

  def adaptSql (exp: SQLExpression) (using LogographConfig) = exp match
      case op: Operation => op.sql
      case _ => s"${exp.sql} = 1"
