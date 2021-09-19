package com.albertoperez1994.scalaql


enum SelectBase [T <: DbDataSet]:
  case Select (select: T)
  case SelectAll (select: T)
  case SelectDistinct  (select: T)
  case SelectDistinctAll  (select: T)

case class Where   (where: Boolean*)
case class OrderBy (orderBy: Any*)

enum BaseJoin:
  case InnerJoin (table: DbDataSet) (join: Boolean*)
  case LeftJoin  (table: DbDataSet) (join: Boolean*)
  case RightJoin (table: DbDataSet) (join: Boolean*)


import SelectBase.*
import BaseJoin.*


case class Query[T <: DbDataSet]()

object Query:

  def apply[T <: DbDataSet] (select: SelectBase[T]) = new Query[T]

  def apply[T <: DbDataSet] (select: SelectBase[T],
                where: Where) = new Query[T]

  def apply[T <: DbDataSet] (select: SelectBase[T],
                orderBy: OrderBy) = new Query[T]

  def apply[T <: DbDataSet] (select: SelectBase[T],
                where: Where,
                orderBy: OrderBy) = new Query[T]

  def apply[T <: DbDataSet] (select: SelectBase[T],
                joins: BaseJoin*) = new Query[T]

  def apply[T <: DbDataSet] (select: SelectBase[T],
                where: Where,
                joins: BaseJoin*) = new Query[T]

  def apply[T <: DbDataSet] (select: SelectBase[T],
                orderBy: OrderBy,
                joins: BaseJoin*) = new Query[T]

  def apply[T <: DbDataSet] (select: SelectBase[T],
                where: Where,
                orderBy: OrderBy,
                joins: BaseJoin*) = new Query[T]
