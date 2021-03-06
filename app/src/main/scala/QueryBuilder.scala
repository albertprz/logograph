package orm

import ast.QueryImpl
import scala.language.experimental.macros
import scala.reflect.runtime.{universe => ru}

case class QueryBuilder[T](private val subqueries: Seq[SelectQuery[_, _]] = Seq.empty) {

  def select[A <: DbDataSet](queryFnTree: T ⇒ Query[A]): SelectQuery[T, A] =
    macro QueryImpl.select[T, A]

  def selectDebug[A <: DbDataSet](queryFnTree: T ⇒ Query[A]): SelectQuery[T, A] =
    macro QueryImpl.selectDebug[T, A]
}

object StatementBuilder {

  def query[T]() = QueryBuilder[T]()

  def query[T](fromQueries: SelectQuery[_, _]*) =
    QueryBuilder[T](subqueries = fromQueries)

  def query[T](fromQuery: SelectQuery[_, T]) =
    QueryBuilder[T](subqueries = Seq(fromQuery))

  def query[T, R](fromQuery1: SelectQuery[_, T], fromQuery2: SelectQuery[_, R]) =
    QueryBuilder[(T, R)](subqueries = Seq(fromQuery1, fromQuery2))

  def query[T, R, X](fromQuery1: SelectQuery[_, T], fromQuery2: SelectQuery[_, R],
                     fromQuery3: SelectQuery[_, X]) =
    QueryBuilder[(T, R, X)](subqueries = Seq(fromQuery1, fromQuery2, fromQuery3))

  def query[T, R, X, S](fromQuery1: SelectQuery[_, T], fromQuery2: SelectQuery[_, R],
                        fromQuery3: SelectQuery[_, X], fromQuery4: SelectQuery[_, S]) =
    QueryBuilder[(T, R, X, S)](subqueries = Seq(fromQuery1, fromQuery2, fromQuery3, fromQuery4))

  def query[T, R, X, S, Q](fromQuery1: SelectQuery[_, T], fromQuery2: SelectQuery[_, R],
                           fromQuery3: SelectQuery[_, X], fromQuery4: SelectQuery[_, S],
                           fromQuery5: SelectQuery[_, Q]) =
    QueryBuilder[(T, R, X, S, Q)](subqueries = Seq(fromQuery1, fromQuery2, fromQuery3,
                                                   fromQuery4, fromQuery5))

  def query[T, R, X, S, Q, H](fromQuery1: SelectQuery[_, T], fromQuery2: SelectQuery[_, R],
                              fromQuery3: SelectQuery[_, X], fromQuery4: SelectQuery[_, S],
                              fromQuery5: SelectQuery[_, Q], fromQuery6: SelectQuery[_, H]) =
    QueryBuilder[(T, R, X, S, Q, H)](subqueries = Seq(fromQuery1, fromQuery2, fromQuery3,
                                                  fromQuery4, fromQuery5, fromQuery6))


  def insert[T <: DbTable](data: T) (implicit tag: ru.TypeTag[T]) =
    InsertStatement (Left (Seq(data)))

  def insert[T <: DbTable](data: Seq[T]) (implicit tag: ru.TypeTag[T]) =
    InsertStatement (Left (data))

  def insert[T <: DbTable](query: SelectQuery[_, T]) (implicit tag: ru.TypeTag[T]) =
    InsertStatement (Right (query))
}
