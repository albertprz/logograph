package com.albertoperez1994.scalaql.utils

import com.albertoperez1994.scalaql.core.Identity
import scala.collection.StringOps
import com.albertoperez1994.scalaql.DbConfig


object StringUtils {

  sealed trait StringCase
  case class CamelCase()      extends StringCase
  case class PascalCase()     extends StringCase
  case class SnakeCase()      extends StringCase
  case class SnakeUpperCase() extends StringCase
  case class KebabCase()      extends StringCase
  case class KebabUpperCase() extends StringCase


  def convertCase (caseConverter: Option[CaseConverter[_, _]], str: String) =
    caseConverter.fold(str)(_.convertCase(str))

  case class CaseConverter[T <: StringCase : FromStringCase, S <: StringCase : ToStringCase] () {

    def convertCase(str: String) =
      (FromStringCase[T].fromCase _).andThen(ToStringCase[S].toCase _)(str)
  }

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

  def splitWhere (str: String, fn: Char => Boolean) = {

    import scala.collection.mutable.ListBuffer
    val indexes = ListBuffer(0)

    while(str.indexWhere(fn, indexes.last + 1) >= 0) {
      indexes += str.indexWhere(fn, indexes.last + 1)
    }

    indexes += str.size

    if (indexes.size == 1)
      Array(str)

    else
      indexes.sliding(2)
             .map(x => str.slice(x(0), x(1)))
             .toArray
  }

  private def getSeparator (value: Any) =
    if (value.toString.size > 30) ",\n " else ", "

  trait PrettyPrintTree {

    override def toString() = show(this, 0)

    private def show(value: Any, depth: Int): String = {

      val result = value match {
        case opt: Option[_] => opt.fold("") (show(_, depth + 1))
        case dict: Map[_, _] => showMap(dict, depth + 1)
        case seq: Seq[_]   => showSeq(seq, depth + 1)
        case prod: Product  => showProduct(prod, depth + 1)
        case other @ _ => other.toString
      }

      result.replace(", ,", ",")
    }

    private def showProduct (prod: Product, depth: Int) = {

      val className = prod.getClass.getSimpleName
      val iter = prod match {
        case ident: Identity => ident.sql()(DbConfig())
        case _ =>  concatStr(prod.productIterator, depth)
      }

      val str = if (!className.contains("Tuple")) s"$className ($iter)"
              else iter
      val indentation = " " * depth

      if (str.size < 25) str else s"\n$indentation$str"
    }

    private def showMap (dict: Map[_, _], depth: Int) = {
      val dictString = dict.asInstanceOf[Map[Any, Any]]
                           .map { case (k, v) => (show(k, depth), show(v, depth)) }
                           .map { case (k, v) => s"$k -> $v" }
                           .mkString(", ")
      s"{$dictString}"
    }

    private def showSeq (seq: Seq[_], depth: Int) = {
      val seqString = concatStr(seq.iterator, depth)
      s"[$seqString]"
    }

    private def concatStr(iter: Iterator[_], depth: Int) =
      iter.map (show(_, depth))
          .mkString(", ")
  }
}

import StringUtils._

trait FromStringCase[T <: StringCase] {
  def fromCase (str: String): Array[String]
}

object FromStringCase {

  def apply[T <: StringCase : FromStringCase] = implicitly[FromStringCase[T]]

  implicit object FromCamelCase extends FromStringCase[CamelCase] {

    def fromCase (str: String) = splitWhere(str, _.isUpper)
  }

  implicit object ToPascalCase extends FromStringCase[PascalCase] {

    def fromCase (str: String) = splitWhere(str, _.isUpper)
  }

  implicit object ToSnakeCase extends FromStringCase[SnakeCase] {

    def fromCase (str: String) = str.split("_")

  implicit object ToUpperSnakeCase extends FromStringCase[SnakeUpperCase] {

    def fromCase (str: String) = str.split("_")
  }

  implicit object ToKebabCase extends FromStringCase[KebabCase] {

    def fromCase (str: String) = str.split("-")
  }

  implicit object ToUpperKebabCase extends FromStringCase[KebabUpperCase] {

    def fromCase (str: String) = str.split("-")  }
  }
}

trait ToStringCase[T <: StringCase] {
  def toCase (splitString: Array[String]): String
}


object ToStringCase {

  def apply[T <: StringCase : ToStringCase] = implicitly[ToStringCase[T]]

  implicit object ToCamelCase extends ToStringCase[CamelCase] {

    def toCase (splitString: Array[String]) = {
      val lower = splitString.map(_.toLowerCase)
      val adaptedList = lower.head +: lower.tail.map(x => x(0).toUpper + x.substring(1))

      adaptedList.mkString("")
    }
  }

  implicit object ToPascalCase extends ToStringCase[PascalCase] {

    def toCase (splitString: Array[String]) =
      splitString.map(_.toLowerCase)
                 .map(x => x(0).toUpper + x.substring(1))
                 .mkString("")
  }

  implicit object ToSnakeCase extends ToStringCase[SnakeCase] {

    def toCase (splitString: Array[String]) =
      splitString.map(_.toLowerCase)
                 .mkString("_")
  }

  implicit object ToUpperSnakeCase extends ToStringCase[SnakeUpperCase] {

    def toCase (splitString: Array[String]) =
      splitString.map(_.toUpperCase)
                 .mkString("_")
  }

  implicit object ToKebabCase extends ToStringCase[KebabCase] {

    def toCase (splitString: Array[String]) =
      splitString.map(_.toLowerCase)
                 .mkString("-")
  }

  implicit object ToUpperKebabCase extends ToStringCase[KebabUpperCase] {

    def toCase (splitString: Array[String]) =
      splitString.map(_.toUpperCase)
                 .mkString("-")
  }
}
