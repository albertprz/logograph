package ast

import scala.reflect.macros.blackbox


class TreeOps [C <: blackbox.Context] (val c: C) {

  import c.universe._


  def findTypedCtorArgs (tree: Tree, className: String) = {

    val ctor = find(tree, t ⇒
      t match {
        case Apply(Apply(TypeApply(Select(x, _), _), _), _) ⇒ x.toString.contains(s".$className")
        case Apply(TypeApply(Select(x, _), _), _) ⇒ x.toString.contains(s".$className")
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

  def findCtorArgs (tree: Tree, className: String) = {

    val ctor = find(tree, t ⇒
      t match {
        case Apply(Apply(Select(x, _), _), _) ⇒ x.toString.contains(s".$className")
        case Apply(Select(x, _), _) ⇒ x.toString.contains(s".$className")
        case _ ⇒ false
      })

    ctor flatMap extractArgTrees
  }

  def getCaseDefArgs (tree: Tree) = {

    val caseDef = find(tree, t ⇒
      t match {
        case CaseDef(x) ⇒ true
        case _ ⇒ false
      })

    caseDef flatMap extractCaseDefArgs
  }

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

  private def find (tree: Tree, filterFn: Tree ⇒ Boolean): List[Tree]  =

    if (filterFn(tree))
      List(tree)

    else if (tree.nonEmpty && tree.children.nonEmpty)
      tree.children
          .flatMap(find (_, filterFn))

    else List.empty
}
