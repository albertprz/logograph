package ast

import orm._
import utils.QueryUtils
import scala.reflect.macros.blackbox
import utils.StringUtils

class QueryExtractor [C <: blackbox.Context] (val c: C) {

  import c.universe._

  private val ops = new TreeOps[c.type](c)
  private var tableAliasMap: Map[String, String] = null

  private def init (tree: Tree, fromTypeName: String) = {
    tableAliasMap = getTableAliasMap(tree, fromTypeName)
  }

  def getUpdateClause (updateTree: Tree, typeName: String) = {

    init(updateTree, typeName)

    val mapArgs = ops.findMapArgs(updateTree)
    val setMap = mapArgs.map { case (key, value) => (getField(key).get, getExpression(value).get)  }
                        .toMap

    val setClause = SetClause(setMap)
    val whereClause = getWhereClause(updateTree)
    val tableName = QueryUtils.splitTupledTypeTag(typeName).head

    val updateClause = UpdateClause(tableName, setClause, whereClause)
    val params = ExpressionClause.findParameters(updateClause)

    (updateClause, params)
  }

  def getDeleteClause (whereTree: Option[Tree], typeName: String) = {

    if (whereTree.isDefined) {
        init(whereTree.get, typeName)
    }

    val whereClause = whereTree.flatMap(getWhereClause)
    val tableName = QueryUtils.splitTupledTypeTag(typeName).head

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

    (queryClause, params)
  }

  private def getSelectClause (tree: Tree) = {

    val args = ops.findTypedCtorArgs(tree, "Select")
                  .flatten
                  .flatMap(getExpression)

    val distinctArgs =  ops.findTypedCtorArgs(tree, "SelectDistinct")
                           .flatten
                           .flatMap(getExpression)


    if      (distinctArgs.nonEmpty)  Some(SelectDistinctClause(args))
    else if (args.nonEmpty)          Some(SelectClause(args))
      else                           None
  }

  private def getWhereClause (tree: Tree) = {

    val args = ops.findCtorArgs(tree, "Where").flatten
                .flatMap(getExpression)

    if (args.nonEmpty)  Some(WhereClause(args))
    else                None
  }

  private def getOrderByClause (tree: Tree) = {

    val args = ops.findCtorArgs (tree, "OrderBy").flatten
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
    val argListsMap = (joinTypes zip joinTypes.map(ops.findCtorArgs(tree, _)))
                        .filter { case (_, argsLists) => argsLists.nonEmpty }
                        .toMap

    val args = for { (joinType, argLists) <- argListsMap
                      argList <- argLists.grouped(2)}
      yield (joinType, getTableAlias(argList(0).head).get, argList(1) flatMap getExpression)

    args.map { case (joinType, tableAlias, exps) => {

        val tableName = tableAliasMap(tableAlias)
        val joinCtor = joinType match {
                  case "InnerJoin" => InnerJoinClause
                  case "RightJoin" => RightJoinClause
                  case "LeftJoin"  => LeftJoinClause
        }

      joinCtor(tableName, tableAlias, exps)
    } }.toList
  }


  private def getExpression(tree: Tree): Option[Expression] = {

    val expressions = List (getField(tree), getOperation(tree), getLiteral(tree),
                                                getIdentity(tree))

    expressions.flatten.headOption
  }

  private def getOperation(tree: Tree) = {

    val op = tree match {
      case q"orm.QueryOps.RichAnyVal[$_]($operand1).$operator($operand2)" =>
        Some((operator, List(operand1, operand2)))
      case q"orm.QueryOps.RichString($operand1).$operator($operand2)" =>
        Some((operator, List(operand1, operand2)))
      case q"orm.QueryOps.$operator[$tpe](..$operands)" =>  Some((operator, operands))
      case q"orm.QueryOps.$operator(..$operands)"       =>  Some((operator, operands))
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

  private def getLiteral(tree: Tree) = tree match {

    case Literal(Constant(value)) =>
      Some(LiteralVal(QueryUtils.convertLiteral(value)))
    case _ => None
  }

  private def getIdentity(tree: Tree) = {

    val identity = tree match {
      case ident @ q"$_.$_.$_.$_.$_" => Some(ident)
      case ident @ q"$_.$_.$_.$_" => Some(ident)
      case ident @ q"$_.$_.$_" => Some(ident)
      case ident @ q"$_.$_" => Some(ident)
      case Ident(ident) => Some(ident.asInstanceOf[Tree])
      case _ => None
    }

    identity map (ident => Identity(ident.toString, ident))
  }

  private def getTableAliasMap (tree: Tree, typeName: String) = {

    val tableAliases = ops.findLambdaFnArgs(tree)
                     .map(_.toString)

    val tableNames = QueryUtils.splitTupledTypeTag(typeName)

    (tableAliases zip tableNames).toMap
  }
}
