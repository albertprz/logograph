package com.albertoperez1994.scalaql.utils

object QueryUtils {

  def splitTupledTypeTag (typeTagStr: String) =

    typeTagStr.replace("(", "").replace(")", "")
      .split(',').map(_.split('.').last).toList

  def convertLiteral(literal: Any): String =
    literal match {
      case str: String   => s"'${str.replace("'", "''")}'"
      case num: Number   => num.toString
      case bool: Boolean => if (bool) "1" else "0"
      case list: List[Any] => list.map(convertLiteral)
                                  .mkString("(", ", ", ")")
      case other: Any      => throw new Exception("Unknown types cannot be used in queries " +
                                                   "for constant or runtime parameter values: \n" +
                                                  s"""|Type: ${literal.getClass.getSimpleName}
                                                      |Value: $literal""".stripMargin)
  }
}
