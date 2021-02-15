package ast

import orm._
import scala.reflect.macros.blackbox


object Functional {
    class PipedObject[T] private[Functional] (value:T)
    {
        def |>[R] (f : T => R) = f(this.value)
    }
    implicit def toPiped[T] (value:T) = new PipedObject[T](value)
}

 import Functional._

class TreeOps [C <: blackbox.Context] (val c: C) {

  import c.universe._


  def getSelectClause (tree: Tree) = {

    val args = (findTypedCtorArgs(tree, "Select").flatten ++ findCtorArgs(tree, "Select").flatten)
                .flatMap(getExpression)

    if (!args.isEmpty)  Some(SelectClause(args))
    else                None
  }

  def getWhereClause (tree: Tree) = {

    val args = findCtorArgs(tree, "Where").flatten
                .flatMap(getOperation)

    if (!args.isEmpty)  Some(WhereClause(args))
    else                None
  }

  def getOrderByClause (tree: Tree) = {

    val args = findCtorArgs (tree, "OrderBy").flatten
                .flatMap(getExpression)

    if (!args.isEmpty)  Some(OrderByClause(args))
    else                None
  }

  def getFromClause (tree: Tree, tableAliases: Map[String, String], joinClauses: List[BaseJoinClause]) = {

    val joinTableAliases = joinClauses map (_.tableAlias)
    val fromTableAliases = tableAliases filter
      { case (tableAlias, _) => !joinTableAliases.contains(tableAlias) }

    if (!tableAliases.isEmpty)  Some(FromClause(fromTableAliases))
    else                        None
  }

  def getJoinClauses (tree: Tree, tableAliases: Map[String, String]) = {

    val joinTypes = List("InnerJoin", "LeftJoin", "RightJoin")
    val argListsMap = (joinTypes zip joinTypes.map (findCtorArgs(tree, _)))
                        .filter { case (_, argsLists) => argsLists.nonEmpty }
                        .toMap

    val args = for ((joinType, argLists) <- argListsMap)
      yield (joinType, getTableAlias(argLists(0).head).get, argLists(1) flatMap getOperation)

    args.map { case (joinType, tableAlias, ops) => {

        val tableName = tableAliases(tableAlias)
        val joinCtor = joinType match {
                  case "InnerJoin" => InnerJoinClause
                  case "RightJoin" => RightJoinClause
                  case "LeftJoin"  => LeftJoinClause
        }
                println(joinType)
                println(joinCtor)
      joinCtor(tableName, tableAlias, ops)
    } }.toList
  }

  def getTableAliases (tree: Tree, typeName: String) = {

    val aliases = getCaseDefArgs(tree)
    val tableNames = splitTupledTypeTag(typeName)

    (aliases zip tableNames).toMap
  }

  private def splitTupledTypeTag (typeTagStr: String) =

    typeTagStr.replace("(", "").replace(")", "")
      .split(',').map(_.split('.').last).toList

  private def getCaseDefArgs (tree: Tree) = {

    val caseDef = find(tree, t ⇒
      t match {
        case CaseDef(x) ⇒ true
        case _ ⇒ false
      })

    caseDef flatMap extractCaseDefArgs
  }

  private def getExpression(tree: Tree): Option[Expression] = {

    val expressions = List[Option[Expression]] (getField(tree), getOperation(tree), getLiteral(tree))

    expressions.flatten.headOption
  }

  private def getOperation(tree: Tree) =

  tree match {
    case Apply(Select(operand, operator), operands) =>
      Some(Operation (operator.decodedName.toString,
                      (operand :: operands).flatMap(getExpression)))

    case _ => None
  }

  private def getField(tree: Tree) =

    tree match {
        case Select(Ident(tableAlias), column) => Some(Field(tableAlias.toString, column.toString))
        case _ => None
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
    }

  private def getIdentity(tree: Tree) =
    tree match {
      case Select(Select(Ident(ident1), ident2), ident3) =>
        Some(Identity(List(ident1, ident2, ident3).map(_.toString)))
      case Select(Ident(ident1), ident2) => Some(Identity(List(ident1, ident2).map(_.toString)))
      case Ident(ident) => Some(Identity(List(ident.toString)))
      case _ => None
    }

  private def findTypedCtorArgs (tree: Tree, className: String) = {

    val ctor = find(tree, t ⇒
      t match {
        case Apply(Apply(TypeApply(Select(Ident(x), _), _), _), _) ⇒ x.toString.contains(className)
        case Apply(TypeApply(Select(Ident(x), _), _), _) ⇒ x.toString.contains(className)
        case _ ⇒ false
      })

    val ctorArgs = ctor flatMap extractArgTrees

    val args = for { argList <- ctorArgs
                    arg <- argList }
      yield arg match {
          case Apply(_, args) => Some(args)
          case _ => None
      }

    args.flatten
  }

  private def findCtorArgs (tree: Tree, className: String) = {

    val ctor = find(tree, t ⇒
      t match {
        case Apply(Apply(Select(Ident(x), _), _), _) ⇒ x.toString.contains(className)
        case Apply(Select(Ident(x), _), _) ⇒ x.toString.contains(className)
        case _ ⇒ false
      })

    ctor flatMap extractArgTrees
  }

  private def find (tree: Tree, filterFn: Tree ⇒ Boolean): List[Tree]  =

    if (filterFn(tree))
      List(tree)

    else if (tree.nonEmpty && tree.children.nonEmpty)
      tree.children
        .flatMap(find (_, filterFn))

    else List.empty


  private def extractArgTrees (tree: Tree)  =

  tree match {
    case Apply(Apply(_, args1), args2) => List(args1, args2)
    case Apply(_, args) ⇒ List(args)
    case _ ⇒ List.empty
  }



  private def extractCaseDefArgs (tree: Tree) = {

    val args = tree match {
      case CaseDef(Apply(_, args), _, _) ⇒ args
      case CaseDef(arg @ Bind(_, _), _, _) ⇒ List(arg)
      case _ ⇒ List.empty
    }

    (for (arg ← args)
      yield arg match {
        case Bind(x, _) ⇒ Some(x.toString)
        case _ ⇒ None
      }
    ).flatten
  }
}
