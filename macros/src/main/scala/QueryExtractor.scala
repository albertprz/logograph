package ast

import orm._
import scala.reflect.macros.blackbox

class QueryExtractor [C <: blackbox.Context] (val c: C) {

  import c.universe._

  private val ops = new TreeOps[c.type](c)
  private var tableAliases: Map[String, String] = null

  private def init (tree: Tree, fromTypeName: String) = {
    tableAliases = getTableAliases(tree, fromTypeName)
  }

  def getQueryClause (tree: Tree, queryTypeName: String) = {

    init(tree, queryTypeName)

    val selectClause = getSelectClause(tree)
    val whereClause = getWhereClause(tree)
    val orderByClause = getOrderByClause(tree)
    val joinClauses = getJoinClauses(tree)
    val fromClause = getFromClause(tree, joinClauses)

     QueryClause (selectClause, whereClause, orderByClause, fromClause, joinClauses)
  }

  private def getSelectClause (tree: Tree) = {

    val args = (ops.findTypedCtorArgs(tree, "Select").flatten ++ ops.findCtorArgs(tree, "Select").flatten)
                .flatMap(getExpression)

    if (!args.isEmpty)  Some(SelectClause(args))
    else                None
  }

  private def getWhereClause (tree: Tree) = {

    val args = ops.findCtorArgs(tree, "Where").flatten
                .flatMap(getExpression)

    if (!args.isEmpty)  Some(WhereClause(args))
    else                None
  }

  private def getOrderByClause (tree: Tree) = {

    val args = ops.findCtorArgs (tree, "OrderBy").flatten
                .flatMap(getExpression)

    if (!args.isEmpty)  Some(OrderByClause(args))
    else                None
  }

  private def getFromClause (tree: Tree, joinClauses: List[BaseJoinClause]) = {

    val joinTableAliases = joinClauses map (_.tableAlias)
    val fromTableAliases = tableAliases filter
      { case (tableAlias, _) => !joinTableAliases.contains(tableAlias) }

    if (!tableAliases.isEmpty)  Some(FromClause(fromTableAliases))
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

        val tableName = tableAliases(tableAlias)
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

  private def getOperation(tree: Tree) =
  tree match {
    case Apply(Select(operand, operator), operands) =>
      Some(Operation (operator.decodedName.toString,
                      (operand :: operands).flatMap(getExpression)))

    case _ => None
  }

  private def getField(tree: Tree) = {

    tree match {
      case q"$tableAlias.$column" if (tableAliases.keySet.contains(tableAlias.toString))
          => Some(Field(tableAlias.toString, column.toString))
        case _ => None
    }
  }

   private def getTableAlias(tree: Tree) =
    tree match {
      case Ident(tableAlias) => Some(tableAlias.toString)
      case _ => None
    }

  private def getLiteral(tree: Tree) =
    tree match {
        case Literal(Constant(value)) => Some(LiteralVal(convertLiteral(value)))
        case _ => None
    }

  private def convertLiteral(literal: Any) =
    literal match {
      case str: String => s"'$str'"
      case num: Number => num.toString
      case bool: Boolean => if (bool) "1" else "0"
      case obj: Any => obj.toString
    }

  private def getIdentity(tree: Tree) = {

    val idents = tree match {
      case q"$ident1.$ident2.$ident3.$ident4.$ident5" => List(ident1, ident2, ident3, ident4, ident5)
      case q"$ident1.$ident2.$ident3.$ident4" => List(ident1, ident2, ident3, ident4)
      case q"$ident1.$ident2.$ident3" => List(ident1, ident2, ident3)
      case q"$ident1.$ident2" => List(ident1, ident2)
      case Ident(ident) => List(ident)
      case _ => List.empty
    }

    if (idents.nonEmpty) Some(Identity(idents map (_.toString)))
    else                 None
  }

  private def getTableAliases (tree: Tree, typeName: String) = {

    val aliases = ops.getCaseDefArgs(tree)
    val tableNames = splitTupledTypeTag(typeName)

    (aliases zip tableNames).toMap
  }

  private def splitTupledTypeTag (typeTagStr: String) =

    typeTagStr.replace("(", "").replace(")", "")
      .split(',').map(_.split('.').last).toList
}
