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

    val args = (findTypedCtorArgs(tree, "Select") ++ findCtorArgs(tree, "Select"))
                .map(getExpression).flatten

    if (!args.isEmpty)  Some(SelectClause(args))
    else                None
  }

  def getWhereClause (tree: Tree) = {

    val args = findCtorArgs(tree, "Where")
                .map(getOperation).flatten

    if (!args.isEmpty)  Some(WhereClause(args))
    else                None
  }

  def getOrderByClause (tree: Tree) = {

    val args = findCtorArgs (tree, "OrderBy")
                .map(getExpression).flatten

    if (!args.isEmpty)  Some(OrderByClause(args))
    else                None
  }

  def getFromClause (tree: Tree, typeName: String) = {

    val aliases = getCaseDefArgs(tree)
    val tableNames = splitTupledTypeTag(typeName)

    if (!aliases.isEmpty)  Some(FromClause(aliases, tableNames))
    else                   None
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

    caseDef.fold(List[String]())(extractCaseDefArgs)
  }

  private def getExpression(tree: Tree): Option[Expression] = {

    val expressions = List[Option[Expression]](getField(tree), getOperation(tree), getLiteral(tree))

    expressions.flatten.headOption
  }

  private def getOperation(tree: Tree) =

  tree match {
    case Apply(Select(operand, operator), operands) =>
      Some(Operation (operator.decodedName.toString,
                      (operand :: operands).map(getExpression).flatten))

    case _ => None
  }

  private def getField(tree: Tree) =

    tree match {
        case Select(Ident(tableAlias), column) => Some(Field(tableAlias.toString, column.toString))
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

  private def findTypedCtorArgs (tree: Tree, className: String) = {

    val ctor = find(tree, t ⇒
      t match {
        case Apply(TypeApply(Select(Ident(x), _), _), _) ⇒ x.toString.contains(className)
        case _ ⇒ false
      })

    val ctorArgs = ctor.fold(List[Tree]())(extractArgTrees)

    val args = for (arg <- ctorArgs)
      yield arg match {
          case Apply(_, args) => Some(args)
          case _ => None
      }

    args.flatten.flatten
  }

  private def findCtorArgs (tree: Tree, className: String) = {

    val ctor = find(tree, t ⇒
      t match {
        case Apply(Select(Ident(x), _), _) ⇒ x.toString.contains(className)
        case _ ⇒ false
      })

    ctor.fold(List[Tree]())(extractArgTrees)
  }

  private def find (tree: Tree, filterFn: Tree ⇒ Boolean): Option[Tree] =

    if (filterFn(tree))
      Some(tree)

    else if (tree.nonEmpty && tree.children.nonEmpty)
      tree.children
        .map(find (_, filterFn))
        .flatten
        .headOption

    else None


  private def extractArgTrees (tree: Tree)  =

  tree match {
    case Apply(_, args) ⇒ args
    case _ ⇒ List.empty[Tree]
  }


  private def extractCaseDefArgs (tree: Tree) = {

    val args = tree match {
      case CaseDef(Apply(_, args), _, _) ⇒ args
      case CaseDef(arg @ Bind(_, _), _, _) ⇒ List(arg)
      case _ ⇒ List.empty[Tree]
    }

    (for (arg ← args)
      yield arg match {
        case Bind(x, _) ⇒ Some(x.toString)
        case _ ⇒ None
      }
    ).flatten
  }
}
