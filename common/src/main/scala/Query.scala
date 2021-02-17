package orm

case class Select [T] (select: T)
case class Where (where: Boolean*)
case class OrderBy  (orderBy: Any*)
case class Join (joins: BaseJoin*)

sealed abstract class BaseJoin()
case class InnerJoin (table: Product) (join: Boolean*) extends BaseJoin
case class LeftJoin (table: Product) (join: Boolean*) extends BaseJoin
case class RightJoin (table: Product) (join: Boolean*) extends BaseJoin

case class Query[T] (private val select: Select[T],
                     private val where: Where = Where(),
                     private val orderBy: OrderBy = OrderBy())
                    (private val joins: BaseJoin*) {

  def where (filterFns: Seq[T ⇒ Boolean]) = ???
}

case class FullQuery [T, R] (private val query: Option[T ⇒ Query[R]] = None,
                              private val subqueries: Seq[FullQuery[_, _]] = Seq.empty,
                              private val queryClauseSql: Option[String] = None,
                              val params: Map[String, Any] = Map.empty) {

  def as [A <: Product] () =
    copy[T, A] (query = query.asInstanceOf[Option[T ⇒ Query[A]]])

  val sql = queryClauseSql.fold("") (replaceParams)


  private def replaceParams(query: String): String =
    params.foldLeft (query) { case (qry, (name, value)) =>
                              qry.replace(name, QueryUtils.convertLiteral(value)) }
}

object QueryUtils {

  def splitTupledTypeTag (typeTagStr: String) =

    typeTagStr.replace("(", "").replace(")", "")
      .split(',').map(_.split('.').last).toList

  def convertLiteral(literal: Any) =
    literal match {
      case str: String   => s"'$str'"
      case num: Number   => num.toString
      case bool: Boolean => if (bool) "1" else "0"
      case obj: Any      => throw new Exception(s"""|Unknown types cannot be used in queries
                                                    |for constant or runtime values \n
                                                    |Type: ${literal.getClass} Value: $literal""".stripMargin)
  }

}
