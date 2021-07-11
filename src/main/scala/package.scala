package com.albertoperez1994

import scala.reflect.runtime.{universe => ru}

import com.albertoperez1994.scalaql.core._
import com.albertoperez1994.scalaql.macros.QueryImpl
import com.albertoperez1994.scalaql.utils.StringUtils.CaseConverter

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

  def deleteDebug[T <: DbTable] (where: T => Where): DeleteStatement[T] =
    macro QueryImpl.deleteDebug[T]

  def update[T <: DbTable] (setMap: T => (Map[Any, Any], Where)): UpdateStatement[T] =
    macro QueryImpl.update[T]

  def updateAll[T <: DbTable] (setMap: T => Map[Any, Any]): UpdateStatement[T] =
    macro QueryImpl.updateAll[T]

  def updateDebug[T <: DbTable] (setMap: T => (Map[Any, Any], Where)): UpdateStatement[T] =
    macro QueryImpl.updateDebug[T]


  // Query factory functions
  case class QueryBuilder[T]() {

    def select[R <: DbDataSet](query: T ⇒ Query[R]): SelectStatement[R] =
      macro QueryImpl.select[T, R]

    def selectDebug[R <: DbDataSet](query: T ⇒ Query[R]): SelectStatement[R] =
      macro QueryImpl.selectDebug[T, R]
  }


  // Query Set Operations
  // def union[T <: DbDataSet] (selects: SelectStatement[T]*) (implicit tag: ru.TypeTag[T]) =
  //   SelectStatement.union(selects)

  // def unionAll[T <: DbDataSet] (selects: SelectStatement[T]*) (implicit tag: ru.TypeTag[T]) =
  //   SelectStatement.unionAll(selects)

  // def intersect[T <: DbDataSet] (selects: SelectStatement[T]*) (implicit tag: ru.TypeTag[T]) =
  //   SelectStatement.intersect(selects)

  // def except[T <: DbDataSet] (selects: SelectStatement[T]*) (implicit tag: ru.TypeTag[T]) =
  //   SelectStatement.except(selects)


  /*
   Available SQL Operations for use in statements
   */

  // Aggregate Functions
  def sum[T <: AnyVal] (x: T): T = ???
  def avg[T <: AnyVal] (x: T): T = ???
  def max[T <: AnyVal] (x: T): T = ???
  def min[T <: AnyVal] (x: T): T = ???
  def stringAgg (x: String): String = ???
  def count (x: Any): Int = ???

  // Coalesce function
  def coalesce [T <: AnyVal] (x: T*): T = ???

  // Not Operator
  def not (x: Boolean): Boolean = ???

  // Order By Functions
  def asc[T <: AnyVal] (x: T): T = ???
  def asc (x: String): String = ???
  def desc[T <: AnyVal] (x: T): T = ???
  def desc (x: String): String = ???


  // Infix Operators for Strings
  implicit class RichString (x: String) {

    def === (y: String): Boolean = ???
    def <>  (y: String): Boolean = ???
    def in (y: Seq[String]): Boolean = ???
    def notIn  (y: Seq[String]): Boolean = ???
    def like (y: String): Boolean = ???
    def notLike (y: String): Boolean = ???
  }

  // Infix Operators for Booleans
  implicit class RichBoolean (x: Boolean) {

    def === (y: Boolean): Boolean = ???
    def <>  (y: Boolean): Boolean = ???
    def and (y: Boolean): Boolean = ???
    def or (y: Boolean): Boolean = ???
  }

  // Infix Operators for Ints
  implicit class RichInt (x: Int) {

    def === (y: Int): Boolean = ???
    def <>  (y: Int): Boolean = ???
    def in  (y: Seq[Int]): Boolean = ???
    def notIn  (y: Seq[Int]): Boolean = ???
  }

  // Infix Operators for Longs
  implicit class RichLong (x: Long) {

    def === (y: Long): Boolean = ???
    def <>  (y: Long): Boolean = ???
    def in  (y: Seq[Long]): Boolean = ???
    def notIn  (y: Seq[Long]): Boolean = ???
  }

  // Infix Operators for BigDecimals
  implicit class RichBigDecimal (x: BigDecimal) {

    def === (y: BigDecimal): Boolean = ???
    def <>  (y: BigDecimal): Boolean = ???
    def in  (y: Seq[BigDecimal]): Boolean = ???
    def notIn  (y: Seq[BigDecimal]): Boolean = ???
  }
}
