package com.albertoperez1994

import com.albertoperez1994.scalaql.core._
import com.albertoperez1994.scalaql.macros.QueryImpl
import com.albertoperez1994.scalaql.utils.ReflectionUtils._

import scala.reflect.runtime.{universe => ru}


package object scalaql {

  import scala.language.experimental.macros

  // Statement factory functions
  def selectAll[T <: DbTable]: SelectStatement[T] =
    macro QueryImpl.selectAll[T]

  def update[T <: DbTable] (setMap: T => (Map[Any, Any], Where)): UpdateStatement[T] =
    macro QueryImpl.update[T]

  def updateAll[T <: DbTable] (setMap: T => Map[Any, Any]): UpdateStatement[T] =
    macro QueryImpl.updateAll[T]

  def delete[T <: DbTable] (where: T => Where): DeleteStatement[T] =
    macro QueryImpl.delete[T]

  def deleteAll[T <: DbTable] (): DeleteStatement[T] =
    macro QueryImpl.deleteAll[T]

  def insert[T <: DbTable] (data: T) (implicit tag: ru.TypeTag[T]) =
    InsertStatement (Left (Seq(data)), extractTypeInfo[T]())

  def insert[T <: DbTable] (data: Seq[T]) (implicit tag: ru.TypeTag[T]) =
    InsertStatement (Left (data), extractTypeInfo[T]())

  def insert[T <: DbTable] (query: SelectStatement[T]) (implicit tag: ru.TypeTag[T]) =
    InsertStatement (Right (query), extractTypeInfo[T]())


  case class QueryBuilder[T](subQueries: Seq[SelectStatement[_]] = Seq.empty) {

    def select[R <: DbDataSet](query: T => Query[R]): SelectStatement[R] =
      macro QueryImpl.selectFrom[T, R]
  }


  def from[T] = QueryBuilder[T]()

  def from[T <: DbDataSet] (fromQuery: SelectStatement[T]) =
      QueryBuilder[T](subQueries = Seq(fromQuery))

  def from[T <: DbDataSet, R <: DbDataSet]
    (fromQuery1: SelectStatement[T], fromQuery2: SelectStatement[R]) =
    QueryBuilder[(T, R)](subQueries = Seq(fromQuery1, fromQuery2))

  def from[T <: DbDataSet, R <: DbDataSet, X <: DbDataSet]
    (fromQuery1: SelectStatement[T], fromQuery2: SelectStatement[R],
     fromQuery3: SelectStatement[X]) =
    QueryBuilder[(T, R, X)](subQueries = Seq(fromQuery1, fromQuery2, fromQuery3))

  def from[T <: DbDataSet, R <: DbDataSet, X <: DbDataSet,
            S <: DbDataSet]
    (fromQuery1: SelectStatement[T], fromQuery2: SelectStatement[R],
     fromQuery3: SelectStatement[X], fromQuery4: SelectStatement[S]) =
    QueryBuilder[(T, R, X, S)](subQueries = Seq(fromQuery1, fromQuery2, fromQuery3,
                                                fromQuery4))

  def from[T <: DbDataSet, R <: DbDataSet, X <: DbDataSet,
            S <: DbDataSet, Q <: DbDataSet]
     (fromQuery1: SelectStatement[T], fromQuery2: SelectStatement[R],
      fromQuery3: SelectStatement[X], fromQuery4: SelectStatement[S],
      fromQuery5: SelectStatement[Q]) =
    QueryBuilder[(T, R, X, S, Q)](subQueries = Seq(fromQuery1, fromQuery2, fromQuery3,
                                                   fromQuery4, fromQuery5))

  def from[T <: DbDataSet, R <: DbDataSet, X <: DbDataSet,
            S <: DbDataSet, Q <: DbDataSet, H <: DbDataSet]
      (fromQuery1: SelectStatement[T], fromQuery2: SelectStatement[R],
       fromQuery3: SelectStatement[X], fromQuery4: SelectStatement[S],
       fromQuery5: SelectStatement[Q], fromQuery6: SelectStatement[H]) =
    QueryBuilder[(T, R, X, S, Q, H)](subQueries = Seq(fromQuery1, fromQuery2, fromQuery3,
                                                      fromQuery4, fromQuery5, fromQuery6))



  // Query Set Operations
  def union[T <: DbDataSet] (selects: SelectStatement[T]*) =
    SelectStatement.union(selects)

  def unionAll[T <: DbDataSet] (selects: SelectStatement[T]*) =
    SelectStatement.unionAll(selects)

  def intersect[T <: DbDataSet] (selects: SelectStatement[T]*) =
    SelectStatement.intersect(selects)

  def except[T <: DbDataSet] (selects: SelectStatement[T]*) =
    SelectStatement.except(selects)


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
  implicit class ScalaQLString (x: String) {

    def === (y: String): Boolean = ???
    def <>  (y: String): Boolean = ???
    def in (y: List[String]): Boolean = ???
    def notIn  (y: List[String]): Boolean = ???
    def like (y: String): Boolean = ???
    def notLike (y: String): Boolean = ???
  }

  // Infix Operators for Booleans
  implicit class ScalaQLBoolean (x: Boolean) {

    def === (y: Boolean): Boolean = ???
    def <>  (y: Boolean): Boolean = ???
    def and (y: Boolean): Boolean = ???
    def or (y: Boolean): Boolean = ???
  }

  // Infix Operators for Ints
  implicit class ScalaQLInt (x: Int) {

    def === (y: Int): Boolean = ???
    def <>  (y: Int): Boolean = ???
    def in  (y: List[Int]): Boolean = ???
    def notIn  (y: List[Int]): Boolean = ???
  }

  // Infix Operators for Longs
  implicit class ScalaQLLong (x: Long) {

    def === (y: Long): Boolean = ???
    def <>  (y: Long): Boolean = ???
    def in  (y: List[Long]): Boolean = ???
    def notIn  (y: List[Long]): Boolean = ???
  }

  // Infix Operators for BigDecimals
  implicit class ScalaQLBigDecimal (x: BigDecimal) {

    def === (y: BigDecimal): Boolean = ???
    def <>  (y: BigDecimal): Boolean = ???
    def in  (y: List[BigDecimal]): Boolean = ???
    def notIn  (y: List[BigDecimal]): Boolean = ???
  }
}
