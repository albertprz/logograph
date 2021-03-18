package orm

import ast.QueryImpl
import scala.language.experimental.macros
import scala.reflect.runtime.{universe => ru}


object StatementBuilder {

  def query[T] = QueryBuilder[T]()

  def selectAll[T <: DbDataSet] (): SelectStatement[T] =
    macro QueryImpl.selectAll[T]

  def insert[T <: DbTable] (data: T) (implicit tag: ru.TypeTag[T]) =
    InsertStatement (Left (Seq(data)))

  def insert[T <: DbTable] (data: Seq[T]) (implicit tag: ru.TypeTag[T]) =
    InsertStatement (Left (data))

  def insert[T <: DbTable] (query: SelectStatement[T]) (implicit tag: ru.TypeTag[T]) =
    InsertStatement (Right (query))

  def delete[T <: DbTable] (where: T => Where): DeleteStatement[T] =
    macro QueryImpl.delete[T]

  def deleteAll[T <: DbTable]: DeleteStatement[T] =
    macro QueryImpl.deleteAll[T]

  def update[T <: DbTable] (setMap: T => (Map[Any, Any], Where)): UpdateStatement[T] =
    macro QueryImpl.update[T]

  def updateAll[T <: DbTable] (setMap: T => Map[Any, Any]): UpdateStatement[T] =
    macro QueryImpl.updateAll[T]
}


case class QueryBuilder[T]() {

  def select[R <: DbDataSet](query: T ⇒ Query[R]): SelectStatement[R] =
    macro QueryImpl.select[T, R]

  def selectDebug[R <: DbDataSet](query: T ⇒ Query[R]): SelectStatement[R] =
    macro QueryImpl.selectDebug[T, R]
}
