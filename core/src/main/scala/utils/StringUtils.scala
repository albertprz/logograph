package com.albertoperez1994.scalaql.utils

import scala.collection.StringOps

object StringUtils {

  def convertCase (caseConverter: Option[CaseConverter], str: String) =
    caseConverter.fold(str)(_.convertCase(str))

  def pprint (value: Any): String = value match {
    case seq: Seq[_]   => seq.map(pprint)
                             .mkString("[", getSeparator(seq), "]")

    case dict: Map[_, _] => dict.map { case (k, v) => (pprint(k),  pprint(v)) }
                                .map { case (k, v) => s"$k -> $v" }
                                .mkString("{", getSeparator(dict), "}")

    case prod: Product   => prod.productIterator
                                .map(pprint)
                                .mkString("(", ", ", ")")
    case null => "null"
    case other @ _ => other.toString
  }

  def stringify (seq: Seq[String]) = seq.mkString("(", ", ", ")")

  private def getSeparator (value: Any) =
    if (value.toString.size > 30) ",\n " else ", "
}
