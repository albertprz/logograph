package com.albertprz.maglor.macros

import scala.reflect.macros.blackbox

import com.albertprz.maglor._
import com.albertprz.maglor.core._
import com.albertprz.maglor.utils.TypeInfo

class QueryImpl(val c: blackbox.Context) {

  import c.universe._

  def selectAll[T <: DbDataSet] (implicit tag: WeakTypeTag[T]): Expr[SelectStatement[T]] =
    buildQueryAll[T] ()

  def select[T, R <: DbDataSet] (query: Tree) (implicit tag1: WeakTypeTag[T], tag2: WeakTypeTag[R]): Expr[SelectStatement[R]] =
    buildQuery[T, R] (query, false)

  def selectFrom[T, R <: DbDataSet] (query: Tree)
                (implicit tag1: WeakTypeTag[T], tag2: WeakTypeTag[R]) : Expr[SelectStatement[R]] =
    buildQuery[T, R] (query, true)

  def updateAll[T <: DbTable] (setMap: Tree) (implicit tag: WeakTypeTag[T]): Expr[UpdateStatement[T]] =
    buildUpdate[T] (setMap)

  def update[T <: DbTable] (setMap: Tree) (implicit tag: WeakTypeTag[T]): Expr[UpdateStatement[T]] =
    buildUpdate[T] (setMap)

  def deleteAll[T <: DbTable] () (implicit tag: WeakTypeTag[T]): Expr[DeleteStatement[T]]  =
    buildDelete[T] ()

  def delete[T <: DbTable] (where: Tree)  (implicit tag: WeakTypeTag[T]): Expr[DeleteStatement[T]]  =
    buildDelete[T] (Some(where))


  private val extractor = new QueryExtractor[c.type](c)

  implicit val typeInfoLiftable = Liftable[TypeInfo] { t =>
    q"utils.TypeInfo(${t.fullClassName}, ${t.className}, ${t.elemNames}, ${t.elemTypes})"
  }


  private def buildQuery[T, R <: DbDataSet] (queryTree: Tree, includeSubQueries: Boolean)
                                            (implicit tag1: WeakTypeTag[T], tag2: WeakTypeTag[R]) = {

    val (typeInfoT, typeInfoR) = (extractTypeInfo[T], extractTypeInfo[R])
    val (clause, params, table) = extractor.getQueryClause(queryTree, typeInfoT.fullClassName,
                                                           typeInfoR.fullClassName, typeInfoR.elemNames)

    emitMessage(clause)

    val subQueries =
      if (includeSubQueries)
        c.Expr[Seq[SelectStatement[_]]] (Select(c.prefix.tree, TermName("subQueries")))
      else
        c.Expr[Seq[SelectStatement[_]]] (q"Seq.empty")


    c.Expr[SelectStatement[R]](q"""SelectStatement(sqlTemplate = ${clause.sql},
                                                   params = ${params.asInstanceOf[Map[String, Tree]]},
                                                   tableNames = ${table.map(_.sql)},
                                                   typeInfo = $typeInfoR,
                                                   subQueries = $subQueries.asInstanceOf[Seq[SelectStatement[DbDataSet]]],
                                                   index = 0,
                                                   dependencies = Seq.empty[Int])""")
  }

  private def buildQueryAll[T <: DbDataSet] () (implicit tag: WeakTypeTag[T]) = {

    val typeInfo = extractTypeInfo[T]

    val (clause, table) = extractor.getQueryClause(weakTypeOf[T].toString)


    c.Expr[SelectStatement[T]](q"""SelectStatement(sqlTemplate = ${clause.sql},
                                                   params = Map.empty[String, Any],
                                                   tableNames = ${List(table.sql)},
                                                   typeInfo = ${typeInfo},
                                                   subQueries = Seq.empty[SelectStatement[DbDataSet]],
                                                   index = 0,
                                                   dependencies = Seq.empty[Int])""")}

  private def buildUpdate[T <: DbTable] (updateTree: Tree)
                         (implicit tag: WeakTypeTag[T]) = {

     val (clause, params) = extractor.getUpdateClause(updateTree, weakTypeOf[T].toString)

    emitMessage(clause)

    c.Expr[UpdateStatement[T]](q"""UpdateStatement(sqlTemplate = ${clause.sql},
                                                   params = ${params.asInstanceOf[Map[String, Tree]]})""")
  }

  private def buildDelete[T <: DbTable] (whereTree: Option[Tree] = None)
                         (implicit tag: WeakTypeTag[T]) = {

     val (clause, params) = extractor.getDeleteClause(whereTree, weakTypeOf[T].toString)

    emitMessage(clause)

    c.Expr[DeleteStatement[T]](q"""DeleteStatement(sqlTemplate = ${clause.sql},
                                                   params = ${params.asInstanceOf[Map[String, Tree]]})""")
  }

  private def emitMessage(clause: SQLClause) = {

    val compilationMessage = s"\n${clause.sql}\n\n\n".stripMargin

      c.info(c.enclosingPosition, compilationMessage, false)
  }


  private def extractTypeInfo[T: WeakTypeTag] = {

    val ctorParamList = weakTypeOf[T].decls
                                     .filter(_.name.decoded == "<init>")
                                     .head.asMethod.paramLists.head

    val elemNames = ctorParamList.map(_.name.decoded)
    val elemTypes = ctorParamList.map(_.info.toString())
    val fullName = weakTypeOf[T].toString()
    val name     = fullName.split('.').last

    val nestedCaseClass = fullName.split(',')
                                  .find(_.split('.').dropRight(1).last.head.isUpper)

    if (nestedCaseClass.nonEmpty) {

      c.error(c.enclosingPosition,
              s"""Maglor Compilation Error:
                  Case classes used in queries must be defined at the top-level within a package scope.
                  Case class '${nestedCaseClass.get}' defined within the scope of an object or class is not supported."""
                +"\n\n")
    }


    TypeInfo(fullName, name, elemNames, elemTypes)
  }
}
