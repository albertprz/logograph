package com.albertoperez1994.scalaql.core

object QueryOps {

  val infixOps = List("-", "+", "*", "/", "===", "<>", "&&", "||", "<", ">", "<=", ">=", "like", "in")

  val postfixOps = List("desc", "asc")

  val prefixOps = List("unary_!", "coalesce", "count", "sum", "avg", "max", "min", "stringAgg")

  val allOps = infixOps ++ postfixOps ++ prefixOps

  val aggOps = List("count", "sum", "avg", "max", "min", "stringAgg")

  val opsConversion = Map("unary_!" -> "not", "&&" -> "and", "||" -> "or", "===" -> "=")
}
