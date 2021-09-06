package com.albertoperez1994.scalaql.macros

import com.albertoperez1994.scalaql.core._
import com.albertoperez1994.scalaql.{utils => utils}
import utils.QueryUtils._
import scala.reflect.macros.blackbox
import com.albertoperez1994.scalaql.config.ScalaQLConfig

class QueryExtractor [C <: blackbox.Context] (val c: C) {

  import c.universe._

  private val ops = new TreeOps[c.type](c)
  private var tableAliasMap: Map[String, String] = null
  private implicit val config: ScalaQLConfig = ScalaQLConfig.get

  import ops._

  def getUpdateClause (updateTree: Tree, typeName: String) = {

    init(updateTree, typeName)

    val mapArgs = findMapArgs(updateTree)
    val setMap = mapArgs.map { case (key, value) => (getField(key).get, getExpression(value).get)  }
                        .toMap

    val setClause = SetClause(setMap)
    val whereClause = getWhereClause(updateTree)
    val tableName = splitTupledTypeTag(typeName).head

    val updateClause = UpdateClause(tableName, setClause, whereClause)
    val params = ExpressionClause.findParameters(updateClause)

    (updateClause, params)
  }

  def getDeleteClause (whereTree: Option[Tree], typeName: String) = {

    if (whereTree.isDefined) {
        init(whereTree.get, typeName)
    }

    val whereClause = whereTree.flatMap(getWhereClause)
    val tableName = splitTupledTypeTag(typeName).head

    val deleteClause = DeleteClause(tableName, whereClause)
    val params = ExpressionClause.findParameters(deleteClause)

    (deleteClause, params)
  }

  def getQueryClause (tree: Tree, typeName: String) = {

    init(tree, typeName)

    val selectClause = getSelectClause(tree)
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

    val field = Field (tableAlias, "*")
    val select = SelectClause (List(field))
    val from = FromClause (Map(tableAlias -> tableName))

    val queryClause = QueryClause (Some(select), Some(from))

    (queryClause, tableName)
  }

  private def getSelectClause (tree: Tree) = {

    val args = findTypedCtorArgs(tree, "Select").flatten
                  .flatMap(getExpression)

    val distinctArgs =  findTypedCtorArgs(tree, "SelectDistinct").flatten
                           .flatMap(getExpression)

    val allArgs =  findCtorArgs(tree, "SelectAll").flatten
                      .flatMap(getTableAlias)

    val distinctAllArgs =  findCtorArgs(tree, "SelectDistinctAll").flatten
                              .flatMap(getTableAlias)

    if      (distinctAllArgs.nonEmpty)  Some(SelectDistinctAllClause(distinctAllArgs.head))
    else if (distinctArgs.nonEmpty)     Some(SelectDistinctClause(distinctArgs))
    else if (allArgs.nonEmpty)          Some(SelectAllClause(allArgs.head))
    else if (args.nonEmpty)             Some(SelectClause(args))
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
      case q"com.albertoperez1994.scalaql.`package`.RichString($operand1).$operator($operand2)" =>
        Some((operator, List(operand1, operand2)))
      case q"com.albertoperez1994.scalaql.`package`.RichBoolean($operand1).$operator($operand2)" =>
        Some((operator, List(operand1, operand2)))
      case q"com.albertoperez1994.scalaql.`package`.RichInt($operand1).$operator($operand2)" =>
        Some((operator, List(operand1, operand2)))
      case q"com.albertoperez1994.scalaql.`package`.RichLong($operand1).$operator($operand2)" =>
        Some((operator, List(operand1, operand2)))
      case q"com.albertoperez1994.scalaql.`package`.RichBigDecimal($operand1).$operator($operand2)" =>
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

    case q"$tableAlias.$column" if (tableAliasMap.keySet.contains(tableAlias.toString)) =>
      Some(Field(tableAlias.toString, column.toString))
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
                            .mkString("(", ", ", ")")))

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

    val tableNames = splitTupledTypeTag(typeName)

    (tableAliases zip tableNames).toMap
  }

  private def init (tree: Tree, fromTypeName: String) = {
    tableAliasMap = getTableAliasMap(tree, fromTypeName)
  }
}
