package orm

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

  private def strIterable (iter: Iterable[Any], depth: Int) = {
    s"[${concatStr(iter.iterator, depth)}]"
  }

  private def concatStr(iter: Iterator[Any], depth: Int) =
    iter.map (str(_, depth))
        .mkString(", ")
}
