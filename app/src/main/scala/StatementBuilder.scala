package orm

import ast.QueryImpl
import scala.language.experimental.macros
import scala.reflect.runtime.{universe => ru}

case class QueryBuilder[T](private val subqueries: Seq[SelectStatement[_]] = Seq.empty) {

  def select[R <: DbDataSet](query: T ⇒ Query[R]): SelectStatement[R] =
    macro QueryImpl.select[T, R]

  def selectDebug[R <: DbDataSet](query: T ⇒ Query[R]): SelectStatement[R] =
    macro QueryImpl.selectDebug[T, R]
}

object StatementBuilder {

  def query[T] (implicit d: DummyImplicit) = QueryBuilder[T]()

  def query[T] (fromQueries: SelectStatement[_]*) =
    QueryBuilder[T](subqueries = fromQueries)

  def query[T <: DbDataSet] (fromQuery: SelectStatement[T]) =
    QueryBuilder[T](subqueries = Seq(fromQuery))

  def query[T <: DbDataSet, R <: DbDataSet]
    (fromQuery1: SelectStatement[T], fromQuery2: SelectStatement[R]) =
    QueryBuilder[(T, R)](subqueries = Seq(fromQuery1, fromQuery2))

  def query[T <: DbDataSet, R <: DbDataSet, X <: DbDataSet]
    (fromQuery1: SelectStatement[T], fromQuery2: SelectStatement[R],
     fromQuery3: SelectStatement[X]) =
    QueryBuilder[(T, R, X)](subqueries = Seq(fromQuery1, fromQuery2, fromQuery3))

  def query[T <: DbDataSet, R <: DbDataSet, X <: DbDataSet, S <: DbDataSet]
    (fromQuery1: SelectStatement[T], fromQuery2: SelectStatement[R],
     fromQuery3: SelectStatement[X], fromQuery4: SelectStatement[S]) =
    QueryBuilder[(T, R, X, S)](subqueries = Seq(fromQuery1, fromQuery2, fromQuery3, fromQuery4))

  def query[T <: DbDataSet, R <: DbDataSet, X <: DbDataSet, S <: DbDataSet, Q <: DbDataSet]
     (fromQuery1: SelectStatement[T], fromQuery2: SelectStatement[R],
      fromQuery3: SelectStatement[X], fromQuery4: SelectStatement[S],
      fromQuery5: SelectStatement[Q]) =
    QueryBuilder[(T, R, X, S, Q)](subqueries = Seq(fromQuery1, fromQuery2, fromQuery3,
                                                   fromQuery4, fromQuery5))

  def query[T <: DbDataSet, R <: DbDataSet, X <: DbDataSet,
            S <: DbDataSet, Q <: DbDataSet, H <: DbDataSet]
      (fromQuery1: SelectStatement[T], fromQuery2: SelectStatement[R],
       fromQuery3: SelectStatement[X], fromQuery4: SelectStatement[S],
       fromQuery5: SelectStatement[Q], fromQuery6: SelectStatement[H]) =
    QueryBuilder[(T, R, X, S, Q, H)](subqueries = Seq(fromQuery1, fromQuery2, fromQuery3,
                                                  fromQuery4, fromQuery5, fromQuery6))

  def select[T <: DbDataSet] (implicit tag: ru.TypeTag[T]) = SelectStatement[T] ()

  def insert[T <: DbTable] (data: T) (implicit tag: ru.TypeTag[T]) =
    InsertStatement (Left (Seq(data)))

  def insert[T <: DbTable] (data: Seq[T]) (implicit tag: ru.TypeTag[T]) =
    InsertStatement (Left (data))

  def insert[T <: DbTable] (query: SelectStatement[T]) (implicit tag: ru.TypeTag[T]) =
    InsertStatement (Right (query))

  def delete[T <: DbTable] : DeleteStatement[T] =
    macro QueryImpl.delete[T]

  def deleteWhere[T <: DbTable] (where: T => Where): DeleteStatement[T] =
    macro QueryImpl.deleteWhere[T]

  def update[T <: DbTable] (setMap: T => Map[Any, Any]): UpdateStatement[T] =
    macro QueryImpl.update[T]

  def updateWhere[T <: DbTable] (setMap: T => (Map[Any, Any], Where)): UpdateStatement[T] =
    macro QueryImpl.updateWhere[T]
}
