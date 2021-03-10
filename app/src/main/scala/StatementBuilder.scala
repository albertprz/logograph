package orm

import ast.QueryImpl
import scala.language.experimental.macros
import scala.reflect.runtime.{universe => ru}

case class QueryBuilder[T](private val subqueries: Seq[SelectQuery[_]] = Seq.empty) {

  def select[R <: DbDataSet](tree: T ⇒ Query[R]): SelectQuery[R] =
    macro QueryImpl.select[T, R]

  def selectDebug[R <: DbDataSet](tree: T ⇒ Query[R]): SelectQuery[R] =
    macro QueryImpl.selectDebug[T, R]
}

object StatementBuilder {

  def query[T] (implicit d: DummyImplicit) = QueryBuilder[T]()

  def query[T] (fromQueries: SelectQuery[_]*) =
    QueryBuilder[T](subqueries = fromQueries)

  def query[T <: DbDataSet] (fromQuery: SelectQuery[T]) =
    QueryBuilder[T](subqueries = Seq(fromQuery))

  def query[T <: DbDataSet, R <: DbDataSet]
    (fromQuery1: SelectQuery[T], fromQuery2: SelectQuery[R]) =
    QueryBuilder[(T, R)](subqueries = Seq(fromQuery1, fromQuery2))

  def query[T <: DbDataSet, R <: DbDataSet, X <: DbDataSet]
    (fromQuery1: SelectQuery[T], fromQuery2: SelectQuery[R],
     fromQuery3: SelectQuery[X]) =
    QueryBuilder[(T, R, X)](subqueries = Seq(fromQuery1, fromQuery2, fromQuery3))

  def query[T <: DbDataSet, R <: DbDataSet, X <: DbDataSet, S <: DbDataSet]
    (fromQuery1: SelectQuery[T], fromQuery2: SelectQuery[R],
     fromQuery3: SelectQuery[X], fromQuery4: SelectQuery[S]) =
    QueryBuilder[(T, R, X, S)](subqueries = Seq(fromQuery1, fromQuery2, fromQuery3, fromQuery4))

  def query[T <: DbDataSet, R <: DbDataSet, X <: DbDataSet, S <: DbDataSet, Q <: DbDataSet]
     (fromQuery1: SelectQuery[T], fromQuery2: SelectQuery[R],
      fromQuery3: SelectQuery[X], fromQuery4: SelectQuery[S],
      fromQuery5: SelectQuery[Q]) =
    QueryBuilder[(T, R, X, S, Q)](subqueries = Seq(fromQuery1, fromQuery2, fromQuery3,
                                                   fromQuery4, fromQuery5))

  def query[T <: DbDataSet, R <: DbDataSet, X <: DbDataSet,
            S <: DbDataSet, Q <: DbDataSet, H <: DbDataSet]
      (fromQuery1: SelectQuery[T], fromQuery2: SelectQuery[R],
       fromQuery3: SelectQuery[X], fromQuery4: SelectQuery[S],
       fromQuery5: SelectQuery[Q], fromQuery6: SelectQuery[H]) =
    QueryBuilder[(T, R, X, S, Q, H)](subqueries = Seq(fromQuery1, fromQuery2, fromQuery3,
                                                  fromQuery4, fromQuery5, fromQuery6))

  def select[T <: DbDataSet] (implicit tag: ru.TypeTag[T]) = SelectQuery[T] ()

  def insert[T <: DbTable] (data: T) (implicit tag: ru.TypeTag[T]) =
    InsertStatement (Left (Seq(data)))

  def insert[T <: DbTable] (data: Seq[T]) (implicit tag: ru.TypeTag[T]) =
    InsertStatement (Left (data))

  def insert[T <: DbTable] (query: SelectQuery[T]) (implicit tag: ru.TypeTag[T]) =
    InsertStatement (Right (query))

  def delete[T <: DbTable] (implicit tag: ru.TypeTag[T]) =
    DeleteStatement [T]

  def update[T <: DbTable] (tree: T => Map[Any, Any]): UpdateStatement[T] =
    macro QueryImpl.update[T]
}
