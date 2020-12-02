package orm

import shapeless._
import shapeless.record._
import shapeless.syntax._
import scala.language.experimental.macros
import scala.reflect.macros.blackbox


// object Extensions {
//   implicit class RichAny [T] (x: T) {

//     def |=  [R <% T] (y: R): Boolean = ???
//     def === [R <% T] (y: R): Boolean = ???

//     def <>  [R <% T] (y: R): Boolean  = ???
//     def !== [R <% T] (y: R): Boolean = ???
//   }

//   implicit class RichAnyVal [T <: AnyVal] (x: T) {

//     def sum (): T = ???
//     def product (): T = ???
//   }

//   implicit class RichString  (x: String) {

//     def stringAgg (): String = ???
//   }
// }

case class BaseQuery[T, R] (private val _select: T ⇒ R = (x: T) ⇒ x,
                            private val _where: Seq[T ⇒ Boolean] = Seq.empty,
                            private val _groupBy: Seq[T ⇒ Any] = Seq.empty,
                            private val _sortBy: Seq[T ⇒ Any] = Seq.empty) {

  def select [A] (mapFn: T ⇒ A) =
    copy(_select = mapFn)

  def where (filterFns: Seq[T ⇒ Boolean]) =
    copy(_where = filterFns)

  def groupBy (groupFns: Seq[T ⇒ Any]) =
    copy(_groupBy = groupFns)

  def sortBy (sortFns: Seq[T ⇒ Any]) =
    copy(_sortBy = sortFns)
}

trait SQLClause {
  val sql: String
}

trait Expression extends SQLClause {
}

case class Field (table: String, column: String) extends Expression {

  val sql = s"$table.$column"
}

abstract class OpType
case class Infix () extends OpType
case class Prefix () extends OpType
case class Postfix () extends OpType


case class Operation (operator: String, operands: List[Expression]) extends Expression {

  val opType = if (Seq("-", "+", "*", "/", "=", "<>", "like").contains(operator))
                 Infix()
               else if  (Seq("desc", "asc").contains(operator))
                 Postfix()
               else
                 Prefix()

  val sql = opType match {

      case Infix() ⇒ operands.map(_.sql)
                              .mkString(s" $operator ")

      case Postfix() ⇒ operands.head.sql + " " + operator

      case Prefix() ⇒ {

        val operandsStr = operands.map(_.sql)
                        .mkString(", ")

        s"$operator ($operandsStr)"
      }
  }
}


case class SelectClause (fields: List[Expression]) extends SQLClause {

  val sql = fields
            .map(_.sql)
            .mkString("SELECT ", ", ", "\n")
}

case class WhereClause (preds: List[Operation]) extends SQLClause {

  val sql = preds
            .map(_.sql)
            .mkString("WHERE ", " AND ", "\n")
}

case class GroupByClause (fields: List[Field]) extends SQLClause {

  val sql = fields
            .map(_.sql)
            .mkString("GROUP BY ", ", ", "\n")
}

case class SortByClause (fields: List[Expression]) extends SQLClause {

  val sql = fields
            .map(_.sql)
            .mkString("SORT BY ", ", ", "\n")
}

case class QueryClause (select: Option[SelectClause], where: Option[WhereClause],
                        groupBy: Option[GroupByClause], sortBy: Option[SortByClause])
                        extends SQLClause {

  val sql = select.fold("")(_.sql) +
            where.fold("")(_.sql) +
            groupBy.fold("")(_.sql) +
            sortBy.fold("")(_.sql)
}


case class Select [T] (select: T)
case class Where (where: Boolean*)
case class GroupBy (groupBy: Any*)
case class SortBy  (sortBy: Any*)

case class Query[T] (private val select: Select[T],
                     private val where: Where = Where(),
                     private val groupBy: GroupBy = GroupBy(),
                     private val sortBy: SortBy = SortBy())


object QueryBuilder {

  def from [T] () = QueryBuilder[T] ()

  def from [T] (fromQueries: FullQuery[_, _]*) =
    QueryBuilder [T] (subqueries = fromQueries)

  def from [T] (fromQuery: FullQuery[_, T]) =
    QueryBuilder [T] (subqueries = Seq(fromQuery))

  def from [T, R] (fromQuery1: FullQuery[_, T], fromQuery2: FullQuery[_, R]) =
    QueryBuilder [(T, R)] (subqueries = Seq(fromQuery1, fromQuery2))

  def from [T, R, X] (fromQuery1: FullQuery[_, T], fromQuery2: FullQuery[_, R],
                      fromQuery3: FullQuery[_, X]) =
    QueryBuilder [(T, R, X)] (subqueries = Seq(fromQuery1, fromQuery2, fromQuery3))

  def from [T, R, X, S] (fromQuery1: FullQuery[_, T], fromQuery2: FullQuery[_, R],
                        fromQuery3: FullQuery[_, X], fromQuery4: FullQuery[_, S]) =
    QueryBuilder [(T, R, X, S)] (subqueries = Seq(fromQuery1, fromQuery2, fromQuery3,
                                                       fromQuery4))

  def from [T, R, X, S, Q] (fromQuery1: FullQuery[_, T], fromQuery2: FullQuery[_, R],
                         fromQuery3: FullQuery[_, X], fromQuery4: FullQuery[_, S],
                         fromQuery5: FullQuery[_, Q]) =
    QueryBuilder [(T, R, X, S, Q)] (subqueries = Seq(fromQuery1, fromQuery2, fromQuery3,
                                                          fromQuery4, fromQuery5))


  def from [T, R, X, S, Q, H] (fromQuery1: FullQuery[_, T], fromQuery2: FullQuery[_, R],
                         fromQuery3: FullQuery[_, X], fromQuery4: FullQuery[_, S],
                         fromQuery5: FullQuery[_, Q], fromQuery6: FullQuery[_, H]) =
    QueryBuilder [(T, R, X, S, Q, H)] (subqueries = Seq(fromQuery1, fromQuery2, fromQuery3,
                                                             fromQuery4, fromQuery5, fromQuery6))
}

abstract class FullQuery [T, R] (query: Any = None,
                                 subqueries: Seq[FullQuery[_, _]] = Seq.empty,
                                 conditions: Seq[T ⇒ Boolean] = Seq.empty)

case class FinalQuery [T, R] (private val query: T ⇒ Query[R],
                              private val subqueries: Seq[FullQuery[_, _]] = Seq.empty,
                              private val conditions: Seq[T ⇒ Boolean] = Seq.empty)
                             extends FullQuery[T, R] {

  def as [A <: Product] () =
    copy[T, A] (query = query.asInstanceOf[T ⇒ Query[A]])

  def gen () = ???
}

case class IntermediateQuery [T, R] (private val query: BaseQuery[T, R],
                                     private val subqueries: Seq[FullQuery[_, _]] = Seq.empty,
                                     private val conditions: Seq[T ⇒ Boolean] = Seq.empty)
                                    extends FullQuery[T, R] {

  def select [A] (mapFn: T ⇒ A) =
    add(_.select(mapFn))

  def where (filterFns: (T ⇒ Boolean)*) =
    add(_.where(filterFns))

  def groupBy (groupFns: (T ⇒ Any)*) =
    add(_.groupBy(groupFns))

  def sortBy (sortFns: (T ⇒ Any)*) =
    add(_.sortBy(sortFns))

  private def add [A] (f: BaseQuery[T, R] ⇒ BaseQuery[T, A]) =
    copy(query = f(query))
}


case class QueryBuilder [T] (private val subqueries: Seq[FullQuery[_, _]] = Seq.empty,
                             private val conditions: Seq[T ⇒ Boolean] = Seq.empty) {

  def on (conds: (T ⇒ Boolean)*) =
    copy(conditions = conds)

  def compile [A] (queryFnTree: T ⇒ Query[A]): String =
    macro QueryImpl.compile

  def create [A] (query: T ⇒ Query[A]) =
    FinalQuery(query, subqueries, conditions)

  def select [A] (mapFn: T ⇒ A) =
    query(_.select(mapFn))

  def where (filterFns: (T ⇒ Boolean)*) =
    query(_.where(filterFns))

  def groupBy (groupFns: (T ⇒ Any)*) =
    query(_.groupBy(groupFns))

  def sortBy (sortFns: (T ⇒ Any)*) =
    query(_.sortBy(sortFns))

  private def query [A] (f: BaseQuery[T, Any] ⇒ BaseQuery[T, A]) =
    IntermediateQuery(f(BaseQuery[T, Any]()), subqueries, conditions)
}


object Application extends App {


  case class Person (name: String, age: Int)
  case class House (street: String)

  var a = 1

  val h = QueryBuilder.from[(Person, House)].compile {
    case (p, h) ⇒ Query(
      Select (Person(p.name, p.age + a)),
      Where (p.name == p.age),
      GroupBy (p.age, p.name)
    ) }


  val j = QueryBuilder.from[Person].compile {
    case p ⇒ Query(
      Select (Person(p.name, p.age + a)),
      Where (p.name == p.age),
      GroupBy (p.age, p.name)
    ) }

  println(h)

}
