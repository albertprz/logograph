package com.albertoperez1994.scalaql.utils

object QueryUtils {

  def splitTupledTypeTag (typeTagStr: String) =
    typeTagStr.replace("(", "")
              .replace(")", "")
              .split(',')
              .map(_.split('.').last)
              .toList

  def literaltoSql (literal: Any): String =
    literal match {
      case str: String   => s"'${str.replace("'", "''")}'"
      case num: Number   => num.toString
      case bool: Boolean => if (bool) "1" else "0"
      case list: List[_]   => list.map(literaltoSql)
                                  .mkString("(", ", ", ")")
      case other: Any    => throw new Exception(s"""|Unknown types cannot be used in queries
                                                    |for constant or runtime parameter values:
                                                    |Type: ${literal.getClass.getSimpleName}
                                                    |Value: $literal""".stripMargin)
    }
}
