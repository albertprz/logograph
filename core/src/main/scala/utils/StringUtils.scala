package com.albertoperez1994.scalaql.utils


object StringUtils:

  def pprint (value: Any): String = value match

    case seq: Seq[?]   => seq.map(pprint)
                             .mkString(getSeparator(seq))
                             .wrapBrackets()

    case dict: Map[?, ?] => dict.map { (k, v) => (pprint(k),  pprint(v)) }
                                .map { (k, v) => s"$k -> $v" }
                                .mkString(getSeparator(dict))
                                .wrapCurlyBrackets()

    case prod: Product   => prod.productIterator
                                .map(pprint)
                                .mkString(", ")
                                .wrapBrackets()
    case null => "null"
    case other @ _ => other.toString


  def stringify (seq: Seq[String]) = seq.mkString("(", ", ", ")")


  private def getSeparator (value: Any) =
    if value.toString.size > 30 then ",\n " else ", "



  extension (str: String)

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

    def convert (converterMap: Option[Map[String, String]]) =
      converterMap.getOrElse(Map.empty)
                  .getOrElse(str, str)

    def betweenChars (start: Char, ending: Char) =
      str.slice(str.indexOf(start) + 1, str.indexOf(ending))

    def wrap (startStr: String, endStr: String) =
      s"$startStr$str$endStr"

    def unwrap() =
      str.substring(1, str.length - 1)

    def wrapBrackets () =
      str.wrap("[", "]")

    def wrapParens () =
      str.wrap("(", ")")

    def wrapCurlyBrackets () =
      str.wrap("{", "}")
