package orm



case class Select [T] (select: T)
case class Where (where: Boolean*)
case class GroupBy (groupBy: Any*)
case class OrderBy  (orderBy: Any*)

case class Query[T] (private val select: Select[T],
                     private val where: Where = Where(),
                     private val groupBy: GroupBy = GroupBy(),
                     private val orderBy: OrderBy = OrderBy())

case class BaseQuery[T, R] (private val _select: T ⇒ R = (x: T) ⇒ x,
                            private val _where: Seq[T ⇒ Boolean] = Seq.empty,
                            private val _groupBy: Seq[T ⇒ Any] = Seq.empty,
                            private val _orderBy: Seq[T ⇒ Any] = Seq.empty) {

  def select [A] (mapFn: T ⇒ A) =
    copy(_select = mapFn)

  def where (filterFns: Seq[T ⇒ Boolean]) =
    copy(_where = filterFns)

  def groupBy (groupFns: Seq[T ⇒ Any]) =
    copy(_groupBy = groupFns)

  def orderBy (sortFns: Seq[T ⇒ Any]) =
    copy(_orderBy = sortFns)
}

abstract class FullQuery [T, R] (private val query: Option[Any] = None,
                                 private val subqueries: Seq[FullQuery[_, _]] = Seq.empty,
                                 private val queryClause: Option[QueryClause] = None)

case class FinalQuery [T, R] (private val query: Option[T ⇒ Query[R]] = None,
                              private val subqueries: Seq[FullQuery[_, _]] = Seq.empty,
                              val queryClause: Option[QueryClause] = None)
                             extends FullQuery[T, R] {

  def as [A <: Product] () =
    copy[T, A] (query = query.asInstanceOf[Option[T ⇒ Query[A]]])

  val sql = queryClause.fold("")(_.sql)
}

case class IntermediateQuery [T, R] (private val query: Option[BaseQuery[T, R]] = None,
                                     private val subqueries: Seq[FullQuery[_, _]] = Seq.empty,
                                     val queryClause: Option[QueryClause] = None)
                                   extends FullQuery[T, R] {

  def select [A] (mapFn: T ⇒ A) =
    add(_.select(mapFn))

  def where (filterFns: (T ⇒ Boolean)*) =
    add(_.where(filterFns))

  def groupBy (groupFns: (T ⇒ Any)*) =
    add(_.groupBy(groupFns))

  def orderBy (sortFns: (T ⇒ Any)*) =
    add(_.orderBy(sortFns))

  private def add [A] (f: BaseQuery[T, R] ⇒ BaseQuery[T, A]) =
    copy(query = query.map(f))

  val sql = queryClause.fold("")(_.sql)
}
