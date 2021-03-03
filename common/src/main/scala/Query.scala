package orm

import utils.QueryUtils
import utils.StringUtils._

sealed abstract class SelectBase [T]
case class Select [T <: DbDataSet] (select: T) extends SelectBase[T]
case class SelectDistinct [T <: DbDataSet] (select: T) extends SelectBase[T]
case class Where (where: Boolean*)
case class OrderBy  (orderBy: Any*)

sealed abstract class BaseJoin
case class InnerJoin (table: DbDataSet) (join: Boolean*) extends BaseJoin
case class LeftJoin (table: DbDataSet) (join: Boolean*) extends BaseJoin
case class RightJoin (table: DbDataSet) (join: Boolean*) extends BaseJoin


sealed abstract class DbDataSet extends Product with Serializable
abstract class DbResult extends DbDataSet

sealed abstract class BaseDbTable extends DbDataSet
abstract class DbTable extends BaseDbTable
abstract class DbTempTable extends BaseDbTable


class Query[T] {

  def where (filterFns: Seq[T ⇒ Boolean]) = ???
}

object Query {

  def apply[T] (select: SelectBase[T]) = new Query[T]

  def apply[T] (select: SelectBase[T],
                where: Where) = new Query[T]

  def apply[T] (select: SelectBase[T],
                orderBy: OrderBy) = new Query[T]

  def apply[T] (select: SelectBase[T],
                where: Where,
                orderBy: OrderBy) = new Query[T]

  def apply[T] (select: SelectBase[T],
                joins: BaseJoin*) = new Query[T]

  def apply[T] (select: SelectBase[T],
                where: Where,
                joins: BaseJoin*) = new Query[T]

  def apply[T] (select: SelectBase[T],
                orderBy: OrderBy,
                joins: BaseJoin*) = new Query[T]

  def apply[T] (select: SelectBase[T],
                where: Where,
                orderBy: OrderBy,
                joins: BaseJoin*) = new Query[T]
}

case class SelectQuery [T, R] (private val query: Option[T ⇒ Query[R]] = None,
                             private val subqueries: Seq[SelectQuery[_, _]] = Seq.empty,
                             private val queryClauseSql: Option[String] = None,
                             val params: Map[String, Any] = Map.empty) {

  val sql = queryClauseSql.fold("") (QueryUtils.replaceParams(_, params))
}
