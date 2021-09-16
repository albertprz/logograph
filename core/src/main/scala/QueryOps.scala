package com.albertoperez1994.scalaql.core

object QueryOps:

  val infixOps = List("-", "+", "*", "/", "===", "<>", "and", "or", "<", ">", "<=", ">=",
                      "like", "notLike", "in", "notIn")

  val postfixOps = List("desc", "asc")

  val prefixOps = List("not", "coalesce", "count", "sum", "avg", "max", "min", "stringAgg")

  val allOps = infixOps ++ postfixOps ++ prefixOps

  val aggOps = List("count", "sum", "avg", "max", "min", "stringAgg")

  val opsConversion = Map("===" -> "=", "notLike" -> "not like", "notIn" -> "not in")
