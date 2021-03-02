package utils

import orm.Identity

object StringUtils {

  def toUnderscoreCase (st: String) =
    splitWhere(st, _.isUpper).mkString("_")

  def splitWhere (st: String, fn: Char => Boolean) = {

    import scala.collection.mutable.ListBuffer
    val indexes = ListBuffer(0)
    var s = st

    while(s.indexWhere(fn) >= 0) {
      indexes += s.indexWhere(fn)
      s = s.substring(indexes.last + 1)
    }

    indexes += st.size

    indexes.sliding(2)
           .map(x => if (x.size == 2) st.slice(x(0), x(1)) else x(0))
           .toList
  }
}

trait PrettyPrint {

  override def toString() = str(this, 0)

  private def str(value: Any, depth: Int): String = {

    val result = value match {
      case opt: Option[Any] => opt.fold("") (str(_, depth + 1))
      case iter: Iterable[Any] => strIterable(iter, depth + 1)
      case prod: Product => strProduct(prod, depth + 1)
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

    val st = if (!className.contains("Tuple")) s"$className ($iter)"
             else iter
    val indentation = " ".repeat(depth)

    if (st.size < 25) st else s"\n$indentation$st"
  }

  private def strIterable (iter: Iterable[Any], depth: Int) =
    s"[${concatStr(iter.iterator, depth)}]"

  private def concatStr(iter: Iterator[Any], depth: Int) =
    iter.map (str(_, depth))
        .mkString(", ")
}
