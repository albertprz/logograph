package com.albertoperez1994

import scala.reflect.runtime.{universe => ru}

import com.albertoperez1994.scalaql.core._
import com.albertoperez1994.scalaql.macros.QueryImpl

package object scalaql {

  import scala.language.experimental.macros


  // Statement factory functions
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


  // Query factory functions
  case class QueryBuilder[T]() {

    def select[R <: DbDataSet](query: T ⇒ Query[R]): SelectStatement[R] =
      macro QueryImpl.select[T, R]

    def selectDebug[R <: DbDataSet](query: T ⇒ Query[R]): SelectStatement[R] =
      macro QueryImpl.selectDebug[T, R]
  }


  /*
   Available SQL Operations for use in statements
   */

  // Aggregate SQL Functions
  def sum[T <: AnyVal] (x: T): T = ???
  def avg[T <: AnyVal] (x: T): T = ???
  def max[T <: AnyVal] (x: T): T = ???
  def min[T <: AnyVal] (x: T): T = ???
  def stringAgg (x: String): String = ???
  def count (x: Any): Int = ???

  // SQL Coalesce function
  def coalesce [T <: AnyVal] (x: T*): T = ???

  // SQL Order By Functions
  def asc[T <: AnyVal] (x: T): T = ???
  def asc (x: String): String = ???
  def desc[T <: AnyVal] (x: T): T = ???
  def desc (x: String): String = ???

  // SQL Infix Functions for Value Types (Number & Booleans)
  implicit class RichAnyVal[T <: AnyVal] (x: T) {

    def === (y: T): Boolean = ???
    def <>  (y: T): Boolean = ???
    def in  (y: Seq[T]): Boolean = ???
  }

  // SQL Infix Functions for Strings
  implicit class RichString (x: String) {

    def === (y: String): Boolean = ???
    def <>  (y: String): Boolean = ???
    def in (y: Seq[String]): Boolean = ???
    def like (y: String): Boolean = ???
  }
}
