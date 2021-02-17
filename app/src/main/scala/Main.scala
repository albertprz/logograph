package orm

import ast.QueryImpl
import scala.language.experimental.macros

object QueryBuilder {

  def query[T]() = QueryBuilder[T]()

  def query[T](fromQueries: FullQuery[_, _]*) =
    QueryBuilder[T](subqueries = fromQueries)

  def query[T](fromQuery: FullQuery[_, T]) =
    QueryBuilder[T](subqueries = Seq(fromQuery))

  def query[T, R](fromQuery1: FullQuery[_, T], fromQuery2: FullQuery[_, R]) =
    QueryBuilder[(T, R)](subqueries = Seq(fromQuery1, fromQuery2))

  def query[T, R, X](fromQuery1: FullQuery[_, T], fromQuery2: FullQuery[_, R],
                     fromQuery3: FullQuery[_, X]) =
    QueryBuilder[(T, R, X)](subqueries = Seq(fromQuery1, fromQuery2, fromQuery3))

  def query[T, R, X, S](fromQuery1: FullQuery[_, T], fromQuery2: FullQuery[_, R],
                        fromQuery3: FullQuery[_, X], fromQuery4: FullQuery[_, S]) =
    QueryBuilder[(T, R, X, S)](subqueries = Seq(fromQuery1, fromQuery2, fromQuery3, fromQuery4))

  def query[T, R, X, S, Q](fromQuery1: FullQuery[_, T], fromQuery2: FullQuery[_, R],
                           fromQuery3: FullQuery[_, X], fromQuery4: FullQuery[_, S],
                           fromQuery5: FullQuery[_, Q]) =
    QueryBuilder[(T, R, X, S, Q)](subqueries = Seq(fromQuery1, fromQuery2, fromQuery3,
                                                   fromQuery4, fromQuery5))

  def query[T, R, X, S, Q, H](fromQuery1: FullQuery[_, T], fromQuery2: FullQuery[_, R],
                              fromQuery3: FullQuery[_, X], fromQuery4: FullQuery[_, S],
                              fromQuery5: FullQuery[_, Q], fromQuery6: FullQuery[_, H]) =
    QueryBuilder[(T, R, X, S, Q, H)](subqueries = Seq(fromQuery1, fromQuery2, fromQuery3,
                                                  fromQuery4, fromQuery5, fromQuery6))


  implicit class RichAny[T] (x: T) {

    def === [R <% T] (y: R): Boolean = ???
    def <>  [R <% T] (y: R): Boolean = ???
  }

  implicit class RichAnyVal [T <: AnyVal] (x: T) {

    def sum (): T = ???
    def product (): T = ???
  }

  implicit class RichString  (x: String) {

    def stringAgg (): String = ???
  }
}

case class QueryBuilder[T](private val subqueries: Seq[FullQuery[_, _]] = Seq.empty) {

  def select[A](queryFnTree: T ⇒ Query[A]): FullQuery[T, A] =
    macro QueryImpl.select[T, A]

  def selectDebug[A](queryFnTree: T ⇒ Query[A]): FullQuery[T, A] =
    macro QueryImpl.selectDebug[T, A]

}

object Application extends App {

  case class Person(name: String, age: Int, houseId: Int, telephoneId: Int)
  case class House(id: Int, street: String)
  case class Telephone(id: Int, number: String)

  import orm.QueryBuilder._

  val a = 3
  val b = Person("", 12, 1, 1)


  val qry = query[(Person, House, Telephone)].selectDebug {
    case (p, h, t) ⇒ Query(
        Select       (p.name, p.age, h.street, t.number, a, b.age),
        Where        (p.age * 2 < 50,
                      h.street <>  "")) (
        LeftJoin (h) (h.id === p.houseId),
        LeftJoin (t) (t.id === p.telephoneId)
      )
  }

  println(qry.sql)
}
