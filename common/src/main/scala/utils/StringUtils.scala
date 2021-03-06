package utils

import orm.Identity
import StringUtils._

object StringUtils {

  case class CamelCase(str: String) extends StringCase
  case class PascalCase (str: String) extends StringCase
  case class SnakeCase(str: String) extends StringCase
  case class SnakeUpperCase(str: String) extends StringCase
  case class KebabCase(str: String) extends StringCase
  case class KebabUpperCase(str: String) extends StringCase

  sealed abstract class StringCase {

    val str: String

    val splitString = this match {
      case CamelCase(str)      => splitWhere(str, _.isUpper)
      case PascalCase(str)     => splitWhere(str, _.isUpper)
      case SnakeCase(str)      => str.split("_")
      case SnakeUpperCase(str) => str.split("_")
      case KebabCase(str)      => str.split("-")
      case KebabUpperCase(str) => str.split("-")
    }

    def toCase [T <: StringCase] (implicit converter: ToStringCase[T]) =
      converter.toCase(splitString)
  }

  def pprint (value: Any): String = value match {
    case seq: Seq[_]   => seq.map(pprint)
                             .mkString("[", if (seq.toString.size > 30) ",\n " else ", ", "]")
    case dict: Map[_, _] => dict.map { case (k, v) => s"$k -> ${pprint(v)}" }.mkString("{", ",\n ", "}")
    case prod: Product   => prod.productIterator.map(pprint).mkString("(", ", ", ")")
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

  trait PrettyPrintTree {

    override def toString() = str(this, 0)

    private def str(value: Any, depth: Int): String = {

      val result = value match {
        case opt: Option[_] => opt.fold("") (str(_, depth + 1))
        case iter: Seq[_]   => strIterable(iter, depth + 1)
        case prod: Product  => strProduct(prod, depth + 1)
        case other @ _ => other.toString
      }

      result.replace(", ,", ",")
    }

    private def strProduct (prod: Product, depth: Int) = {

      val className = prod.getClass.getSimpleName
      val iter = prod match {
        case ident: Identity => ident.sql
        case _ =>  concatStr(prod.productIterator, depth)
      }

      val str = if (!className.contains("Tuple")) s"$className ($iter)"
              else iter
      val indentation = " ".repeat(depth)

      if (str.size < 25) str else s"\n$indentation$str"
    }

    private def strIterable (iter: Iterable[Any], depth: Int) =
      s"[${concatStr(iter.iterator, depth)}]"

    private def concatStr(iter: Iterator[Any], depth: Int) =
      iter.map (str(_, depth))
          .mkString(", ")
  }
}

trait ToStringCase[T <: StringCase] {
  def toCase (splitString: Array[String]): String
}


object ToStringCase {

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
