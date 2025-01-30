package com.albertprz.logograph.utils

import com.albertprz.logograph.utils.StringUtils.*
import java.beans.Expression
import com.albertprz.logograph.core.{Field, SQLExpression}

object Error:

  class ConfigError(val message: String) extends Exception(message)

  case class CaseNotAllowed(elem: String, elemType: String, set: Set[?])
      extends ConfigError(
        s"Element $elem does not belong to the set of allowed $elemType: $set"
      )

  case class InvalidConfig()
      extends ConfigError("Logograph config is not valid")

  class SQLError(val message: String) extends Exception(message)

  case class InvalidOperator(operator: String, validOperators: Set[String])
      extends SQLError(
        s"Operator $operator does not belong to the set of allowed operators: $validOperators"
      )

  case class InvalidLiteralType(literal: Any)
      extends SQLError(s"""|Unknown types cannot be used in queries
                           |for constant or runtime parameter values:
                           |Type: ${literal.getClass.getSimpleName}
                           |Value: $literal""".stripMargin)

  case class InvalidParameterType(value: Any)
      extends SQLError(
        "Unknown types cannot be used in queries " +
          "for constant or runtime parameter values: \n" +
          s"""|Type: ${value.getClass.getName()}
                          |Value: $value""".stripMargin
      )

  case class InvalidQueryResultType(value: Any)
      extends SQLError(
        "Unknown types cannot be returned " +
          "as query results: \n" +
          s"Type: ${value.getClass.getName()}".stripMargin
      )

  class OrderByError(message: String) extends SQLError(message)

  case class InvalidExpression()
      extends OrderByError(
        "Only fields and operations are allowed in an Order By Clause"
      )

  class QueryError(message: String) extends SQLError(message)

  case class NoSelectClause()
      extends QueryError("No Select Clause was found for the current query")

  case class OverlappedAggregatedFields(overlappedFields: List[Field])
      extends QueryError(
        """|\nThere are some fields which are being used both in aggregate functions and
                            | as regular columns:\n${pprint(overlappedFields)}
                            |Please only use these fields within an aggregation function \n""".stripMargin
      )

  case class UnknownOrderByExprs(exprs: List[SQLExpression])
      extends QueryError(
        """|\nThere are some expressions used in the Order By Clause, which were not
                            |included in the Select Clause:\n${pprint(orderByExprs)}
                            |Please only use expressions already included in the Select clause
                            |for the Order By clause if the query is using
                            | a Group By or Distinct clause \n""".stripMargin
      )
