
package orm

trait ExpressionClause extends SQLClause {
  val exprs: List[Expression]
}

sealed abstract class BaseSelectClause extends ExpressionClause

case class SelectClause (exprs: List[Expression]) extends BaseSelectClause {

  val validate = {}

  val sql = exprs
            .map(_.sql)
            .mkString("SELECT      ", ", ", "\n")
}

case class SelectDistinctClause (exprs: List[Expression]) extends BaseSelectClause {

  val validate = {}

  val sql = exprs
            .map(_.sql)
            .mkString("SELECT      DISTINCT ", ", ", "\n")
}

case class FromClause (tableAliases: Map[String, String]) extends SQLClause {

  val validate = {}

  val sql = tableAliases
              .map { case (tableAlias, tableName) => s"[$tableName] AS $tableAlias" }
              .mkString("FROM        ", ", ", "\n")
}

case class WhereClause (exprs: List[Expression]) extends ExpressionClause {

  val validate = {}

  val sql = exprs
            .map(Predicate.adaptSql)
            .map(str => if (exprs.size > 1) s"($str)" else str)
            .mkString("WHERE       ", " AND \n            ", "\n")
}

case class GroupByClause (fields: List[Field]) extends SQLClause {

  val validate = {}

  val sql = fields
            .map(_.sql)
            .mkString("GROUP BY    ", ", ", "\n")
}

case class HavingClause (exprs: List[Expression]) extends ExpressionClause {

  val validate = {}

  val sql = exprs
            .map(Predicate.adaptSql)
            .map(str => if (exprs.size > 1) s"($str)" else str)
            .mkString("HAVING      ", " AND \n            ", "\n")
}

case class OrderByClause (exprs: List[Expression]) extends ExpressionClause {

  val validate = {

    for (expr <- exprs)
      expr match {
        case fld: Field => {}
        case op: Operation => {}
        case _ => throw new Exception(s"Only fields and operations are allowed in an Order By Clause")
      }
  }

  val sql = exprs
            .map(_.sql)
            .mkString("ORDER BY    ", ", ", "\n")
}

sealed abstract class BaseJoinClause extends ExpressionClause {

  val tableName: String
  val tableAlias: String
  val exprs: List[Expression]

  val joinType = this match {
    case inner: InnerJoinClause => "INNER JOIN"
    case left: LeftJoinClause   => "LEFT JOIN "
    case right: RightJoinClause => "RIGHT JOIN"
  }

  val validate = {}

  val sql = exprs
            .map(Predicate.adaptSql)
            .map(str => if (exprs.size > 1) s"($str)" else str)
            .mkString(s"$joinType  [$tableName] AS $tableAlias ON ", " AND \n            ", "\n")
}

case class InnerJoinClause (tableName: String, tableAlias: String, exprs: List[Expression])
    extends BaseJoinClause

case class LeftJoinClause (tableName: String, tableAlias: String, exprs: List[Expression])
    extends BaseJoinClause

case class RightJoinClause (tableName: String, tableAlias: String, exprs: List[Expression])
    extends BaseJoinClause

case class QueryClause (select: Option[BaseSelectClause] = None, from: Option[FromClause] = None,
                        joins: List[BaseJoinClause] = List.empty, wher: Option[WhereClause] = None,
                        orderBy: Option[OrderByClause] = None)
                        extends SQLClause {

  import QueryClause._

  private val aggFields = (List(select, wher, orderBy) flatMap getAggFields) ++ getAggFields(joins)
  private val nonAggFields = (List(select, wher, orderBy) flatMap getNonAggFields) ++ getNonAggFields(joins)
  private val allFields = (List(select, wher, orderBy) flatMap getAllFields) ++ getAllFields(joins)

  private val groupBy = {

    val nonAggFields = allFields.distinct diff aggFields.distinct

     if (aggFields.nonEmpty && nonAggFields.nonEmpty) Some(GroupByClause(nonAggFields))
     else None
  }

  private val where = {

    val nonAggPreds = wher.fold(List.empty[Expression])(_.exprs.filter(_.aggFields.isEmpty))

     wher map (_.copy(exprs = nonAggPreds))
  }

  private val having = {

    val aggPreds = wher.fold(List.empty[Expression])(_.exprs.filter(_.aggFields.nonEmpty))

     if (aggPreds.nonEmpty) Some(HavingClause(aggPreds))
     else None
  }

  val validate = {

    val overlappedFields = aggFields intersect nonAggFields

    if (overlappedFields.nonEmpty)
      throw new Exception("\nThere are some fields which are being used both in aggregate functions and " +
                          s"as regular columns:\n${overlappedFields.mkString("[", ", ", "]")} \n" +
                          "Please only use these fields within an aggregation function \n")
  }

  val sql = {

      select.fold("")(_.sql)           +
      from.fold("")(_.sql)             +
      joins.map(_.sql)
           .mkString("")               +
      where.fold("")(_.sql)            +
      groupBy.fold("")(_.sql)          +
      having.fold("")(_.sql)           +
      orderBy.fold("")(_.sql)
  }
}


object QueryClause {

  def findParameters(model: Any): Map[String, Any] =

     model match {
      case ident: Identity => ident.parameter
      case prod: Product => prod.productIterator.map(findParameters).fold(Map.empty)(_++_)
      case iter: Iterable[Any] => iter.map(findParameters).fold(Map.empty)(_++_)
      case _ => Map.empty
    }

  private def getAllFields(clause: Option[ExpressionClause]) =
      clause.fold (List.empty[Field]) (_.exprs.flatMap(_.fields))

  private def getAllFields(clause: List[ExpressionClause]) =
      clause.flatMap (_.exprs.flatMap(_.fields))

  private def getAggFields(clause: Option[ExpressionClause]) =
      clause.fold (List.empty[Field]) (_.exprs.flatMap(_.aggFields))

  private def getAggFields(clause: List[ExpressionClause]) =
      clause.flatMap (_.exprs.flatMap(_.aggFields))

  private def getNonAggFields(clause: Option[ExpressionClause]) =
      clause.fold (List.empty[Field]) (_.exprs.flatMap(_.nonAggFields))

  private def getNonAggFields(clause: List[ExpressionClause]) =
      clause.flatMap (_.exprs.flatMap(_.nonAggFields))

}
