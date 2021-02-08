package orm

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

  def getGroupByClause (tree: Tree) = {

    val args = findCtorArgs (tree, "GroupBy")
                .map(getField).flatten

    if (!args.isEmpty)  Some(GroupByClause(args))
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

  def splitTupledTypeTag (typeTagStr: String) =

    typeTagStr.replace("(", "").replace(")", "")
      .split(',').map(_.split('.').last).toList

  def getCaseDefArgs (tree: Tree) = {

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

class QueryImpl (val c: blackbox.Context) {

  import c.universe._

  implicit val liftExpression = Liftable[Expression] {exp =>
    exp match {
      case fld: Field => q"Field(${fld.tableAlias}, ${fld.column})"
      case op: Operation => q"Operation(${op.operator}, List[Expression].empty)"
      case ltr: LiteralVal => q"LiteralVal(${ltr.value})"
    }
  }

  implicit val liftField = Liftable[Field] { fld =>
    q"Field(${fld.tableAlias}, ${fld.column})"
  }

  implicit val liftOperation = Liftable[Operation] { op =>
    q"Operation(${op.operator}, ${op.operands})"
  }

  implicit val liftSelect = Liftable[SelectClause] { select =>
    q"SelectClause(${select.exprs})"
  }

  implicit val liftWhere = Liftable[WhereClause] { where =>
    q"WhereClause(${where.preds})"
  }

  implicit val liftGroupBy = Liftable[GroupByClause] { groupBy =>
    q"GroupByClause(${groupBy.fields})"
  }

  implicit val liftOrderBy = Liftable[OrderByClause] { orderBy =>
    q"OrderByClause(${orderBy.exprs})"
  }

  implicit val liftFrom = Liftable[FromClause] { from =>
    q"FromClause(${from.aliases}, ${from.tableNames})"
  }


  def compile [T: WeakTypeTag, R: WeakTypeTag] (queryFnTree: Tree) = {

    val ops = new TreeOps [c.type] (c)

    val selectClause = ops.getSelectClause(queryFnTree)
    val whereClause = ops.getWhereClause(queryFnTree)
    val groupByClause = ops.getGroupByClause(queryFnTree)
    val orderByClause = ops.getOrderByClause(queryFnTree)
    val fromClause = ops.getFromClause(queryFnTree, weakTypeOf[T].toString)


    c.Expr[FinalQuery[T, R]](q"""FinalQuery(queryClause =
                                 Some(QueryClause($selectClause, $whereClause,
                                                  $groupByClause, $orderByClause,
                                                  $fromClause)))""")
  }

}
