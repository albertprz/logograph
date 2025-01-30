package com.albertprz.logograph.utils

import Error.{InvalidLiteralType, SQLError}

object QueryUtils:

  def splitTupledTypeTag(typeTagStr: String) =

    typeTagStr
      .replace("(", "")
      .replace(")", "")
      .split(',')
      .map(_.split('.').last)
      .toList

  def literaltoSql(literal: Any): Either[SQLError, String] =

    literal match
      case str: String   => Right(s"'${str.replace("'", "''")}'")
      case num: Number   => Right(num.toString)
      case bool: Boolean => Right(if bool then "1" else "0")
      case list: List[?] =>
        Right(
          list
            .map(literaltoSql)
            .mkString("(", ", ", ")")
        )
      case other: Any    => Left(InvalidLiteralType(other))
