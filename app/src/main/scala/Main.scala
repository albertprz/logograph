package orm

import ast.QueryImpl
import scala.language.experimental.macros
import scala.language.implicitConversions

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
}

case class QueryBuilder[T](private val subqueries: Seq[FullQuery[_, _]] = Seq.empty) {

  def select[A](queryFnTree: T ⇒ Query[A]): FullQuery[T, A] =
    macro QueryImpl.select[T, A]

  def selectDebug[A](queryFnTree: T ⇒ Query[A]): FullQuery[T, A] =
    macro QueryImpl.selectDebug[T, A]

}

trait Table extends Product with Serializable {

  val * = this
}

object QueryOps {

    def sum[T <: AnyVal] (x: T): T = ???
    def avg[T <: AnyVal] (x: T): T = ???
    def max[T <: AnyVal] (x: T): T = ???
    def min[T <: AnyVal] (x: T): T = ???
    def stringAgg (x: String): String = ???

    def isNull[T] (x: T, y: T): Boolean = ???
    def count (x: Any): Int = ???

    def asc[T] (x: T): T = ???
    def desc[T] (x: T): T = ???
}


object Application extends App {

  case class Person(name: String, age: Int, houseId: Int, telephoneId: Int)
  case class House(id: Int, street: String)
  case class Telephone(id: Int, number: String)

  import orm.QueryBuilder._
  import orm.QueryOps._

  val a = 3
  val b = Person("", 12, 1, 1)
  val cond = a > 0


  val qry = query[(Person, House, Telephone)].select {
    case (p, h, t) ⇒ Query(
          Select       (stringAgg(p.name), sum(p.age), isNull(h.street, ""), t.number, b.age),
          Where        (p.age * a < 50,
                        h.street !=  "",
                        cond),
          OrderBy      (asc (p.name), desc(p.age))) (
          LeftJoin (h) (h.id == p.houseId),
          LeftJoin (t) (t.id == p.telephoneId)
        )
  }


  println()
  println(qry.sql)
  println(qry.params)
  println()
}
