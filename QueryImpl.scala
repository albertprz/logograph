package orm

import scala.reflect.macros.blackbox

class TreeOps [C <: blackbox.Context] (val c: C) {

  import c.universe._

    def find (tree: Tree, filterFn: Tree ⇒ Boolean): Option[Tree] =

      if (filterFn(tree))
        Some(tree)

      else if (tree.nonEmpty && tree.children.nonEmpty)
        tree.children
          .map(find (_, filterFn))
          .flatten
          .headOption

      else None

  def findTypedCtorArgs (tree: Tree, className: String) = {

    val constr = find(tree, t ⇒
      t match {
        case Apply(TypeApply(Select(Ident(x), _), _), _) ⇒ x.toString.contains(className)
        case _ ⇒ false
      })

    for (x ← constr)
      yield extractArgs(x)
  }

  def findCtorArgs (tree: Tree, className: String) = {

    val constr = find(tree, t ⇒
      t match {
        case Apply(Select(Ident(x), _), _) ⇒ x.toString.contains(className)
        case _ ⇒ false
      })

    for (x ← constr)
      yield extractArgs(x)
  }

  def extractArgs (tree: Tree)  =

  tree match {
    case Apply(_, args) ⇒ args
    case _ ⇒ List.empty[Tree]
  }

  def findCaseDefArgs (tree: Tree) = {

    val caseDef = find(tree, t ⇒
      t match {
        case CaseDef(x) ⇒ true
        case _ ⇒ false
      })

    for (x ← caseDef)
      yield extractCaseDefArgs(x)
  }


  def extractCaseDefArgs (tree: Tree) = {

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

class QueryImpl (val c: blackbox.Context) {

  import c.universe._

  def compile (queryFnTree: Tree) = {

    val ops = new TreeOps [c.type] (c)

    val aliases = ops.findCaseDefArgs(queryFnTree).get
    val selectClause = ops.findTypedCtorArgs(queryFnTree, "Select")
    val whereClause = ops.findCtorArgs(queryFnTree, "Where")
    val groupByClause = ops.findCtorArgs(queryFnTree, "GroupBy")
    val sortByClause = ops.findCtorArgs(queryFnTree, "SortBy")

    // val x = groupByClause.toString
    val x = aliases.mkString("[ ", " ", " ]")

    c.Expr[String](q"$x")
  }

}
