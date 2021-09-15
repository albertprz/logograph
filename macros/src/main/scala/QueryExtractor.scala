package com.albertoperez1994.scalaql.macros

import com.albertoperez1994.scalaql.core._
import com.albertoperez1994.scalaql.{utils => utils}
import utils.QueryUtils._
import utils.StringUtils._
import scala.reflect.macros.blackbox
import com.albertoperez1994.scalaql.config.ScalaQLConfig

class QueryExtractor [C <: blackbox.Context] (val c: C) {

  import c.universe._

  private val ops = new TreeOps[c.type](c)
  private var tableAliasMap: Map[String, Table] = null
  private implicit val config: ScalaQLConfig = ScalaQLConfig.get

  import ops._

  def getUpdateClause (updateTree: Tree, typeName: String) = {

    init(updateTree, typeName)

    val mapArgs = findMapArgs(updateTree)
    val setMap = mapArgs.map { case (key, value) => (getField(key).get, getExpression(value).get)  }
                        .toMap

    val table = Table(splitTupledTypeTag(typeName).head)
    val setClause = SetClause(setMap)
    val whereClause = getWhereClause(updateTree)

    val updateClause = UpdateClause(table, setClause, whereClause)
    val params = ExpressionClause.findParameters(updateClause)

    (updateClause, params)
  }

  def getDeleteClause (whereTree: Option[Tree], typeName: String) = {

    if (whereTree.isDefined) {
        init(whereTree.get, typeName)
    }

    val whereClause = whereTree.flatMap(getWhereClause)
    val table = Table(splitTupledTypeTag(typeName).head)

    val deleteClause = DeleteClause(table, whereClause)
    val params = ExpressionClause.findParameters(deleteClause)

    (deleteClause, params)
  }

  def getQueryClause (tree: Tree, fromTypeName: String, selectTypeName: String, columnAliases: List[String]) = {

    init(tree, fromTypeName)

    val selectTableName = splitTupledTypeTag(selectTypeName).head

    val selectClause = getSelectClause(tree, columnAliases, selectTableName)
    val whereClause = getWhereClause(tree)
    val orderByClause = getOrderByClause(tree)
    val joinClauses = getJoinClauses(tree)
    val fromClause = getFromClause(tree, joinClauses)

    val queryClause = QueryClause (selectClause, fromClause, joinClauses, whereClause, orderByClause)
    val params = ExpressionClause.findParameters(queryClause)

    (queryClause, params, tableAliasMap.values.toList)
  }

  def getQueryClause (typeName: String) = {

    val tableName = typeName.split('.').last
    val tableAlias = tableName.head.toLower.toString
    val table = Table(tableName)

    val select = SelectAllClause (tableAlias)
    val from = FromClause (Map(tableAlias -> table))

    val queryClause = QueryClause (Some(select), Some(from), List.empty, None, None)

    (queryClause, table)
  }

  private def getSelectClause (tree: Tree, columnAliases: List[String], selectTableName: String) = {

    val args = findTypedCtorArgs(tree, "Select").flatten
                  .flatMap(getExpression)

    val distinctArgs =  findTypedCtorArgs(tree, "SelectDistinct").flatten
                           .flatMap(getExpression)

    val allArgs =  findCtorArgs(tree, "SelectAll").flatten
                      .flatMap(getTableAlias)

    val distinctAllArgs =  findCtorArgs(tree, "SelectDistinctAll").flatten
                              .flatMap(getTableAlias)

    val columns = columnAliases.map(Column(_, selectTableName))


    if      (distinctAllArgs.nonEmpty)  Some(SelectDistinctAllClause(distinctAllArgs.head))
    else if (distinctArgs.nonEmpty)     Some(SelectDistinctClause(distinctArgs, columns))
    else if (allArgs.nonEmpty)          Some(SelectAllClause(allArgs.head))
    else if (args.nonEmpty)             Some(SelectClause(args, columns))
    else                                None
  }

  private def getWhereClause (tree: Tree) = {

    val args = findCtorArgs(tree, "Where").flatten
                  .flatMap(getExpression)

    if (args.nonEmpty)  Some(WhereClause(args))
    else                None
  }

  private def getOrderByClause (tree: Tree) = {

    val args = findCtorArgs (tree, "OrderBy").flatten
                  .flatMap(getExpression)

    if (args.nonEmpty)  Some(OrderByClause(args))
    else                None
  }

  private def getFromClause (tree: Tree, joinClauses: List[BaseJoinClause] = List.empty) = {

    val joinTableAliases = joinClauses map (_.tableAlias)
    val fromTableAliases = tableAliasMap filter
      { case (tableAlias, _) => !joinTableAliases.contains(tableAlias) }

    if (tableAliasMap.nonEmpty)  Some(FromClause(fromTableAliases))
    else                        None
  }

  private def getJoinClauses (tree: Tree) = {

    val joinTypes = List("InnerJoin", "LeftJoin", "RightJoin")

    val argListsMap = (joinTypes zip joinTypes.map(findCtorArgs(tree, _)))
                        .filter { case (_, argsLists) => argsLists.nonEmpty }
                        .toMap

    val args = for { (joinType, argLists) <- argListsMap
                      argList <- argLists.grouped(2)}
      yield (joinType, getTableAlias(argList(0).head).get, argList(1) flatMap getExpression)

    args.map { case (joinType, tableAlias, exps) =>
        BaseJoinClause (joinType) (config) (tableAliasMap(tableAlias), tableAlias, exps)
     }.toList
  }


  private def getExpression(tree: Tree): Option[Expression] = {

    val expressions = List (getField(tree), getOperation(tree), getLiteral(tree),
                                                getIdentity(tree))

    expressions.flatten.headOption
  }

  private def getOperation(tree: Tree) = {

    val op = tree match {
      case q"com.albertoperez1994.scalaql.`package`.ScalaQLString($operand1).$operator($operand2)" =>
        Some((operator, List(operand1, operand2)))
      case q"com.albertoperez1994.scalaql.`package`.ScalaQLBoolean($operand1).$operator($operand2)" =>
        Some((operator, List(operand1, operand2)))
      case q"com.albertoperez1994.scalaql.`package`.ScalaQLInt($operand1).$operator($operand2)" =>
        Some((operator, List(operand1, operand2)))
      case q"com.albertoperez1994.scalaql.`package`.ScalaQLLong($operand1).$operator($operand2)" =>
        Some((operator, List(operand1, operand2)))
      case q"com.albertoperez1994.scalaql.`package`.ScalaQLBigDecimal($operand1).$operator($operand2)" =>
        Some((operator, List(operand1, operand2)))
      case q"com.albertoperez1994.scalaql.`package`.$operator[$tpe](..$operands)" =>  Some((operator, operands))
      case q"com.albertoperez1994.scalaql.`package`.$operator(..$operands)"       =>  Some((operator, operands))
      case q"$operand.$operator(..$operands)"           =>  Some((operator, operand +: operands))
      case _ => None
    }


    for ((operator, operands) <- op)
      yield Operation(operator.decodedName.toString, operands.flatMap(getExpression))
  }

  private def getField(tree: Tree) = tree match {

    case q"$tableAlias.$columnName" if (tableAliasMap.keySet.contains(tableAlias.toString())) => {

      val table = tableAliasMap(tableAlias.toString())
      val column = Column(columnName.toString(), table.tableName)

      Some(Field(tableAlias.toString(), column))
    }
    case _ => None
  }

  private def getTableAlias(tree: Tree) = tree match {

    case Ident(tableAlias) => Some(tableAlias.toString)
    case _ => None
  }

  private def getLiteral(tree: Tree): Option[LiteralVal] = tree match {

    case Literal(Constant(value))                                                               =>
      Some(LiteralVal(literaltoSql(value)))

    case q"scala.`package`.List.apply[$_](..$values)" if values.forall(getLiteral(_).isDefined) =>
      Some(LiteralVal(values.flatMap(getLiteral(_).map(_.value))
                            .mkString(", ")
                            .wrapParens()))

    case _ => None
  }

  private def getIdentity(tree: Tree) = {

    val identity = tree match {
      case ident @ q"$_.$_.$_.$_.$_" => Some(ident)
      case ident @ q"$_.$_.$_.$_" => Some(ident)
      case ident @ q"$_.$_.$_" => Some(ident)
      case ident @ q"$_.$_" => Some(ident)
      case ident @ Ident(_) => Some(ident.asInstanceOf[Tree])
      case _ => None
    }

    identity map (ident => Identity(ident.toString, ident))
  }

  private def getTableAliasMap (tree: Tree, typeName: String) = {

    val tableAliases = findLambdaFnArgs(tree)
                     .map(_.toString)

    val tableNames = splitTupledTypeTag(typeName).map(Table(_))

    (tableAliases zip tableNames).toMap
  }

  private def init (tree: Tree, fromTypeName: String) = {
    tableAliasMap = getTableAliasMap(tree, fromTypeName)
  }
}
