package orm

import scala.language.experimental.macros


object QueryBuilder {

  def from [T] () = QueryBuilder[T] ()

  def from [T] (fromQueries: FullQuery[_, _]*) =
    QueryBuilder [T] (subqueries = fromQueries)

  def from [T] (fromQuery: FullQuery[_, T]) =
    QueryBuilder [T] (subqueries = Seq(fromQuery))

  def from [T, R] (fromQuery1: FullQuery[_, T], fromQuery2: FullQuery[_, R]) =
    QueryBuilder [(T, R)] (subqueries = Seq(fromQuery1, fromQuery2))

  def from [T, R, X] (fromQuery1: FullQuery[_, T], fromQuery2: FullQuery[_, R],
                      fromQuery3: FullQuery[_, X]) =
    QueryBuilder [(T, R, X)] (subqueries = Seq(fromQuery1, fromQuery2, fromQuery3))

  def from [T, R, X, S] (fromQuery1: FullQuery[_, T], fromQuery2: FullQuery[_, R],
                        fromQuery3: FullQuery[_, X], fromQuery4: FullQuery[_, S]) =
    QueryBuilder [(T, R, X, S)] (subqueries = Seq(fromQuery1, fromQuery2, fromQuery3,
                                                       fromQuery4))

  def from [T, R, X, S, Q] (fromQuery1: FullQuery[_, T], fromQuery2: FullQuery[_, R],
                         fromQuery3: FullQuery[_, X], fromQuery4: FullQuery[_, S],
                         fromQuery5: FullQuery[_, Q]) =
    QueryBuilder [(T, R, X, S, Q)] (subqueries = Seq(fromQuery1, fromQuery2, fromQuery3,
                                                          fromQuery4, fromQuery5))


  def from [T, R, X, S, Q, H] (fromQuery1: FullQuery[_, T], fromQuery2: FullQuery[_, R],
                         fromQuery3: FullQuery[_, X], fromQuery4: FullQuery[_, S],
                         fromQuery5: FullQuery[_, Q], fromQuery6: FullQuery[_, H]) =
    QueryBuilder [(T, R, X, S, Q, H)] (subqueries = Seq(fromQuery1, fromQuery2, fromQuery3,
                                                             fromQuery4, fromQuery5, fromQuery6))
}

case class QueryBuilder [T] (private val subqueries: Seq[FullQuery[_, _]] = Seq.empty) {


  def create [A] (queryFnTree: T ⇒ Query[A]): FinalQuery[T, A] =
    macro QueryImpl.compile[T, A]

  def select [A] (mapFn: T ⇒ A) =
    query(_.select(mapFn))

  def where (filterFns: (T ⇒ Boolean)*) =
    query(_.where(filterFns))

  def groupBy (groupFns: (T ⇒ Any)*) =
    query(_.groupBy(groupFns))

  def orderBy (sortFns: (T ⇒ Any)*) =
    query(_.orderBy(sortFns))

  private def query [A] (f: BaseQuery[T, Any] ⇒ BaseQuery[T, A]) =
    IntermediateQuery(Some(f(BaseQuery[T, Any]())), subqueries)
}



object Application extends App {

  case class Person (name: String, age: Int)
  case class House (street: String)


  val h = QueryBuilder.from[(Person, House)].create {
    case (p, h) ⇒ Query(
      Select (Person(p.name, p.age)),
      Where (p.name == ""),
      GroupBy (p.age)
    ) }


  println()
  println(h.sql)

}
