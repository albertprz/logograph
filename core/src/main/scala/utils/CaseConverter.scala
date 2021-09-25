package com.albertprz.maglor.utils

import Error.CaseNotAllowed
import com.albertprz.maglor.utils.StringUtils.*


case class BiCaseConverter (from: StringCase, to: StringCase):

  def convertCase(str: String): String =
    (from.fromCase _).andThen(to.toCase _)(str)


case class CaseConverter(to: StringCase):

  import StringCase.*

  def convertCase(str: String): String =

    val splitString = for  split1 <- CamelCase.fromCase(str)
                           split2 <- KebabCase.fromCase(split1)
                           split3 <- SnakeCase.fromCase(split2)
                      yield split3

    to.toCase(splitString)


sealed trait StringCase:
  def fromCase (str: String): Array[String]
  def toCase (splitString: Array[String]): String


object StringCase:

  val stringCases = Set("CamelCase", "PascalCase", "SnakeCase",
                        "SnakeUpperCase", "KebabCase", "KebabUpperCase")


  def apply(caseName: String): Either[CaseNotAllowed, StringCase] =
    caseName.normalizedToLower() match
      case "camelcase"      => Right(CamelCase)
      case "pascalcase"     => Right(PascalCase)
      case "snakecase"      => Right(SnakeCase)
      case "snakeuppercase" => Right(SnakeUpperCase)
      case "kebabcase"      => Right(KebabCase)
      case "kebabuppercase" => Right(KebabUpperCase)
      case _                => Left(CaseNotAllowed(caseName, StringCase.toString(), stringCases))

  case object CamelCase extends StringCase:

    def fromCase (str: String) = splitWhere(str, _.isUpper)

    def toCase (splitString: Array[String]) =
      val lower = splitString.map(_.toLowerCase)
      val adaptedList = lower.head +: lower.tail.map(x => s"${x(0).toUpper}${x.substring(1)}")

      adaptedList.mkString("")

  case object PascalCase extends StringCase:

    def fromCase (str: String) = splitWhere(str, _.isUpper)

    def toCase (splitString: Array[String]) =
      splitString.map(_.toLowerCase)
                  .map(x => s"${x(0).toUpper}${x.substring(1)}")
                  .mkString("")

  case object SnakeCase extends StringCase:

    def fromCase (str: String) = str.split("_")

    def toCase (splitString: Array[String]) =
      splitString.map(_.toLowerCase)
                  .mkString("_")

  case object SnakeUpperCase extends StringCase:

    def fromCase (str: String) = str.split("_")

    def toCase (splitString: Array[String]) =
      splitString.map(_.toUpperCase)
                  .mkString("_")

  case object KebabCase extends StringCase:

    def fromCase (str: String) = str.split("-")

    def toCase (splitString: Array[String]) =
      splitString.map(_.toLowerCase)
                  .mkString("-")

  case object KebabUpperCase extends StringCase:

    def fromCase (str: String) = str.split("-")

    def toCase (splitString: Array[String]) =
      splitString.map(_.toUpperCase)
                  .mkString("-")


  private def splitWhere (str: String, fn: Char => Boolean) =

    import scala.collection.mutable.ListBuffer
    val indexes = ListBuffer(0)

    while str.indexWhere(fn, indexes.last + 1) >= 0 do
      indexes += str.indexWhere(fn, indexes.last + 1)

    indexes += str.size

    if indexes.size == 1 then
      Array(str)

    else
      indexes.sliding(2)
             .map(x => str.slice(x(0), x(1)))
             .toArray
