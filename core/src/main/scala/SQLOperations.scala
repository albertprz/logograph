package com.albertprz.logograph.core

object SQLOperations:

  val infixOps = Set("-", "+", "*", "/", "===", "<>", "and", "or", "<", ">", "<=", ">=",
                      "like", "notLike", "in", "notIn")

  val postfixOps = Set("desc", "asc")

  val prefixOps = Set("not", "coalesce", "count", "sum", "avg", "max", "min", "stringAgg")

  val allOps = infixOps ++ postfixOps ++ prefixOps

  val aggOps = Set("count", "sum", "avg", "max", "min", "stringAgg")

  val opsConversion = Map("===" -> "=", "notLike" -> "not like", "notIn" -> "not in")
