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
                              val queryClause: Option[QueryClause] = None) {

  def as [A <: Product] () =
    copy[T, A] (query = query.asInstanceOf[Option[T ⇒ Query[A]]])

  val sql = queryClause.fold("")(_.sql)
}
