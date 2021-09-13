package com.albertoperez1994.scalaql.utils

// import scala.collection.StringOps

object StringUtils {

  def pprint (value: Any): String = value match {
    case seq: Seq[_]   => seq.map(pprint)
                             .mkString(getSeparator(seq))
                             .wrapBrackets()

    case dict: Map[_, _] => dict.map { case (k, v) => (pprint(k),  pprint(v)) }
                                .map { case (k, v) => s"$k -> $v" }
                                .mkString(getSeparator(dict))
                                .wrapCurlyBrackets()

    case prod: Product   => prod.productIterator
                                .map(pprint)
                                .mkString(", ")
                                .wrapBrackets()
    case null => "null"
    case other @ _ => other.toString
  }

  def stringify (seq: Seq[String]) = seq.mkString("(", ", ", ")")

  private def getSeparator (value: Any) =
    if (value.toString.size > 30) ",\n " else ", "


  implicit class RichString (str: String) {

    def mapLines(mapFn: String => String) = str.split("\n")
                                               .map(mapFn)
                                               .mkString("\n")

    def trimLines() = str.mapLines(_.trim())
                         .trim()

    def normalizedToLower() = str.replaceAll("-", "")
                                 .replaceAll("_", "")
                                 .toLowerCase()

    def indent (spacing: Int) = str.mapLines(" " * spacing + _)

    def convertCase (caseConverter: Option[CaseConverter]) =
      caseConverter.fold(str)(_.convertCase(str))

    def convert (converterMap: Map[String, String]) =
      converterMap.getOrElse(str, str)

    def wrap (startStr: String, endStr: String) =
      s"$startStr$str$endStr"

    def unwrap() =
      str.substring(1, str.length - 1)

    def wrapBrackets () =
      wrap("[", "]")

    def wrapParens () =
      wrap("(", ")")

    def wrapCurlyBrackets () =
      wrap("{", "}")
  }
}
