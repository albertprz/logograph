package com.albertoperez1994.scalaql.macros

import com.albertoperez1994.scalaql.core.*
import com.albertoperez1994.scalaql.{utils => utils}
import utils.QueryUtils.*
import utils.StringUtils.*
import com.albertoperez1994.scalaql.config.ScalaQLConfig

import scala.quoted.*

class QueryExtractor  (val quotes: Quotes):

  private var tableAliasMap: Map[String, Table] = null
  given cfg: ScalaQLConfig = ScalaQLConfig.get
  given Quotes = quotes

  import quotes.reflect.*


  def getUpdateClause (updateTree: Tree, typeName: String) =

    init(updateTree, typeName)

    val mapArgs = findMapArgs(updateTree)
    val setMap = mapArgs.map { case (key, value) => (getField(key).get, getExpressions(value).head)  }
                        .toMap

    val table = Table(splitTupledTypeTag(typeName).head)
    val setClause = SetClause(setMap)
    val whereClause = getWhereClause(updateTree)

    val updateClause = UpdateClause(table, setClause, whereClause)
    val params = ExpressionClause.findParameters(updateClause)

    (updateClause, params)


  def getDeleteClause (whereTree: Option[Tree], typeName: String) =

    if whereTree.isDefined then
        init(whereTree.get, typeName)

    val whereClause = whereTree.flatMap(getWhereClause)
    val table = Table(splitTupledTypeTag(typeName).head)

    val deleteClause = DeleteClause(table, whereClause)
    val params = ExpressionClause.findParameters(deleteClause)

    (deleteClause, params)


  def getQueryClause (tree: Tree, fromTypeName: String, selectTypeName: String, columnAliases: List[String]) =

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


  def getQueryClause (typeName: String) =

    val tableName = typeName.split('.').last
    val tableAlias = tableName.head.toLower.toString
    val table = Table(tableName)

    val select = SelectAllClause (tableAlias)
    val from = FromClause (Map(tableAlias -> table))

    val queryClause = QueryClause (Some(select), Some(from), List.empty, None, None)

    (queryClause, table)


  private def getSelectClause (tree: Tree, columnAliases: List[String], selectTableName: String) =

    val args = findTypedCtorArgs(tree, "Select").flatten
                  .flatMap(getExpressions)

    val distinctArgs =  findTypedCtorArgs(tree, "SelectDistinct").flatten
                           .flatMap(getExpressions)

    val allArgs =  findCtorArgs(tree, "SelectAll").flatten
                      .flatMap(getTableAlias)

    val distinctAllArgs =  findCtorArgs(tree, "SelectDistinctAll").flatten
                              .flatMap(getTableAlias)

    val columns = columnAliases.map(Column(_, selectTableName))


    if      distinctAllArgs.nonEmpty then  Some(SelectDistinctAllClause(distinctAllArgs.head))
    else if distinctArgs.nonEmpty then     Some(SelectDistinctClause(distinctArgs, columns))
    else if allArgs.nonEmpty then          Some(SelectAllClause(allArgs.head))
    else if args.nonEmpty then             Some(SelectClause(args, columns))
    else                                   None


  private def getWhereClause (tree: Tree) =

    val args = findCtorArgs (tree, "Where").flatten
                  .flatMap(getExpressions)

    if args.nonEmpty then  Some(WhereClause(args))
    else                   None


  private def getOrderByClause (tree: Tree) =

    val args = findCtorArgs (tree, "OrderBy").flatten
                  .flatMap(getExpressions)

    if args.nonEmpty then  Some(OrderByClause(args))
    else                   None


  private def getFromClause (tree: Tree, joinClauses: List[BaseJoinClause] = List.empty) =

    val joinTableAliases = joinClauses map (_.tableAlias)
    val fromTableAliases = tableAliasMap filter
      { case (tableAlias, _) => !joinTableAliases.contains(tableAlias) }

    if tableAliasMap.nonEmpty then  Some(FromClause(fromTableAliases))
    else                            None


  private def getJoinClauses (tree: Tree) =

    val joinTypes = List("InnerJoin", "LeftJoin", "RightJoin")

    val argListsMap = (joinTypes zip joinTypes.map(findCtorArgs(tree, _)))
                        .filter { case (_, argsLists) => argsLists.nonEmpty }
                        .toMap


    val args = for  (joinType, argLists) <- argListsMap
                      argList <- argLists.grouped(2).filter(_.size > 1)
      yield (joinType, getTableAlias(argList(0).head).get, argList(1) flatMap getExpressions)


    args.map { case (joinType, tableAlias, exps) =>
        BaseJoinClause (joinType) (tableAliasMap(tableAlias), tableAlias, exps)
     }.toList


  private def getExpressions(tree: Tree): List[Expression] =


    val exprTrees = tree match {
      case block: Typed =>  getSeqLiteralElems(block.expr)
      case x            =>  List(x)
    }


    val expressions = for exprTree <- exprTrees
                        yield List (getField(exprTree), getOperation(exprTree),
                                    getLiteral(exprTree), getIdentity(exprTree))
                          .flatten.headOption

    expressions.flatten



  private def getOperation(tree: Tree) =


    val op: Option[(String, List[Term])] = tree match
      case Apply(TypeApply(Ident(operator), _), operands)       =>  Some((operator, operands))
      case Apply(Apply(Ident(operator), operands1), operands2)  =>  Some((operator, operands1 ++ operands2))
      case Apply(Select(operand, operator), operands)           =>  Some((operator, operand +: operands))
      case _ => None


    for (operator, operands) <- op
      yield Operation(operator, operands.flatMap(getExpressions))


  private def getField(tree: Tree) = tree match

    case Select(Ident(tableAlias), columnName)
      if (tableAliasMap.keySet.contains(tableAlias.toString())) => {

      val table = tableAliasMap(tableAlias.toString())
      val column = Column(columnName.toString(), table.tableName)

      Some(Field(tableAlias.toString(), column))
    }
    case _ => None


  private def getTableAlias(tree: Tree) = tree match

    case Ident(tableAlias) => Some(tableAlias.toString)
    case _ => None


  private def getLiteral(tree: Tree): Option[LiteralVal] =

    tree match

    case Literal(constant) =>
       Some(LiteralVal(literaltoSql(constant.value)))

    case Apply(TypeApply(Select(Ident(id), _), _), List(block))  if id.toString.contains("List") =>

       val list = getSeqLiteralElems(block.asInstanceOf[Typed].expr)
                      .flatMap(getLiteral(_).map(_.value))
                      .mkString(", ")
                      .wrapParens()

       Some(LiteralVal(list))

    case _ => None


  private def getIdentity(tree: Tree) =

    val identity = tree match
      case ident @ Select(Select(Select(Select(_, _), _), _), _)    => Some(ident)
      case ident @ Select(Select(Select(_, _), _), _)               => Some(ident)
      case ident @ Select(Select(_, _), _)                          => Some(ident)
      case ident @ Select(_, _)                                     => Some(ident)
      case ident @ Ident(_)                                         => Some(ident.asInstanceOf[Tree])
      case _                                                        => None

    identity map (ident => Identity(ident.toString, ident))


  private def getTableAliasMap (tree: Tree, typeName: String) =

    val tableAliases = findLambdaFnArgs(tree)
                     .map(_.toString)

    val tableNames = splitTupledTypeTag(typeName).map(Table(_))

    (tableAliases zip tableNames).toMap


  private def init (tree: Tree, fromTypeName: String) =
    tableAliasMap = getTableAliasMap(tree, fromTypeName)


  def getSeqLiteralElems (seqLiteral: Any) =
    seqLiteral.asInstanceOf[Product].productIterator.toList.head
              .asInstanceOf[List[Tree]]


  def findMapArgs (tree: Tree) =

    // println(tree.show)

    val mapEntries = findAll(tree) (_ match {
        case Apply(TypeApply(Select(Apply(TypeApply(Ident(arrow), _), _), _), _), _) =>
          arrow.contains("ArrowAssoc")
        case _                    => false
    } )

    for mapEntry <- mapEntries
      yield mapEntry match
        case Apply(TypeApply(Select(Apply(_, key), _), _), value) => (key.head, value.head)


  def findTypedCtorArgs (tree: Tree, className: String) =

    val ctor = findAll(tree) (t => t match {
        case Apply(Apply(TypeApply(Select(Ident(x), _), _), _), _) => x.toString.contains(className)
        case Apply(TypeApply(Select(Ident(x), _), _), _)           => x.toString.contains(className)
        case _                                                     => false
      })


    val ctorArgs = ctor flatMap extractFnArgs

    val args = for  argList <- ctorArgs
                    arg <- argList
      yield arg match
          case Apply(_, args) => Some(args)
          case _              => None

    args.flatten



  def findCtorArgs (tree: Tree, className: String) =

    val ctor = findAll(tree) (t => t match {
        case Apply(Apply(TypeApply(Select(Ident(x), _), _), _), _) => x.toString.contains(className)
        case Apply(TypeApply(Select(Ident(x), _), _), _)           => x.toString.contains(className)
        case Apply(Apply(Select(Ident(x), _), _), _)               => x.toString.contains(className)
        case Apply(Select(Ident(x), _), _)                         => x.toString.contains(className)
        case _                                                     => false
      })

    ctor flatMap extractFnArgs


  def findLambdaFnArgs (tree: Tree) =

    val lambdaFn = findAll(tree) (t => t match {
        case _: DefDef | _: CaseDef | _: ValDef => true
        case _                      => false
      })

    (lambdaFn flatMap extractDefDefArgs) ++
    (lambdaFn flatMap extractCaseDefArgs) ++
    (lambdaFn flatMap extractValDefArgs)


  private def extractCaseDefArgs (tree: Tree) =

    val args = tree match
      case CaseDef(caseDef, _, _) => findAll(caseDef) ( _ match {
        case _: Bind => true
        case _       => false
      } )
      case _ => List.empty

    (for arg <- args
      yield arg match {
        case Bind(arg, _) if !arg.contains("$") => Some(arg)
        case _ => None
      }
    ).flatten


  private def extractDefDefArgs (tree: Tree) =
    tree match
      case  DefDef(arg, _, _, _) if !arg.contains("$") => List(arg)
      case _ => List.empty


  private def extractValDefArgs (tree: Tree) =
    tree match
      case ValDef(arg, _ , _) if !arg.contains("$") => List(arg)
      case _ => List.empty


  private def extractFnArgs (tree: Tree) =
    tree match
      case Apply(Apply(_, args1), args2) => List(args1, args2)
      case Apply(_, args) => List(args)
      case _ => List.empty


  private def findAll (tree: Tree)(filterFn: Tree => Boolean): List[Tree] =

    val acc = new TreeAccumulator[List[Tree]] {
      def foldTree(matchedTrees: List[Tree], tree: Tree)(owner: Symbol): List[Tree] =

        val current =
          if filterFn(tree) then  List (tree)
          else                    Nil

        foldOverTree(matchedTrees ++ current, tree)(owner)
    }

    acc.foldOverTree(Nil, tree) (tree.symbol)
