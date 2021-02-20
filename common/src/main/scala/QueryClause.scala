
package orm

case class SelectClause (exprs: List[Expression]) extends SQLClause {

  val sql = exprs
            .map(_.sql)
            .mkString("SELECT      ", ", ", "\n")
}

case class WhereClause (preds: List[Expression]) extends SQLClause {

  val sql = preds
            .map(Predicate.adaptSql)
            .map(str => if (preds.size > 1) s"($str)" else str)
            .mkString("WHERE       ", " AND \n            ", "\n")
}

case class GroupByClause (fields: List[Field]) extends SQLClause {

  val sql = fields
            .map(_.sql)
            .mkString("GROUP BY    ", ", ", "\n")
}

case class OrderByClause (exprs: List[Expression]) extends SQLClause {

  val sql = exprs
            .map(_.sql)
            .mkString("ORDER BY    ", ", ", "\n")
}

case class FromClause (tableAliases: Map[String, String]) extends SQLClause {

  val sql = tableAliases
              .map { case (tableAlias, tableName) => s"[$tableName] AS $tableAlias" }
              .mkString("FROM        ", ", ", "\n")
}

sealed abstract class BaseJoinClause extends SQLClause {

  val tableName: String
  val tableAlias: String
  val preds: List[Expression]

  val joinType = this match {
    case inner: InnerJoinClause => "INNER JOIN"
    case left: LeftJoinClause   => "LEFT JOIN "
    case right: RightJoinClause => "RIGHT JOIN"
  }

  val sql = preds
            .map(Predicate.adaptSql)
            .map(str => if (preds.size > 1) s"($str)" else str)
            .mkString(s"$joinType  [$tableName] AS $tableAlias ON ", " AND \n            ", "\n")
}

case class InnerJoinClause (tableName: String, tableAlias: String, preds: List[Expression])
    extends BaseJoinClause

case class LeftJoinClause (tableName: String, tableAlias: String, preds: List[Expression])
    extends BaseJoinClause

case class RightJoinClause (tableName: String, tableAlias: String, preds: List[Expression])
    extends BaseJoinClause

object QueryClause {

  def findParameters(model: Any): Map[String, Any] =

     model match {
      case ident: Identity => ident.parameters
      case prod: Product => prod.productIterator.map(findParameters).fold(Map.empty)(_++_)
      case iter: Iterable[Any] => iter.map(findParameters).fold(Map.empty)(_++_)
      case _ => Map.empty
    }
}

case class QueryClause (select: Option[SelectClause] = None, from: Option[FromClause] = None,
                        joins: List[BaseJoinClause] = List.empty, where: Option[WhereClause] = None,
                        orderBy: Option[OrderByClause] = None)
                        extends SQLClause {

  private val groupBy = {

    val nonAggFields = select.fold (List.empty[Field]) (_.exprs.flatMap(_.nonAggFields))
    val aggFields = select.fold (List.empty[Field]) (_.exprs.flatMap(_.aggFields))

     if (!aggFields.isEmpty && !nonAggFields.isEmpty) Some(GroupByClause(nonAggFields))
     else None
  }

  val sql = {

      select.fold("")(_.sql)           +
      from.fold("")(_.sql)             +
      joins.map(_.sql)
           .mkString("", "", "\n")     +
      where.fold("")(_.sql)            +
      groupBy.fold("")(_.sql)          +
      orderBy.fold("")(_.sql)
  }
}
