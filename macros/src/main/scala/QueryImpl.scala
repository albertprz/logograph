package com.albertoperez1994.scalaql.macros

import scala.reflect.macros.blackbox

import com.albertoperez1994.scalaql._
import com.albertoperez1994.scalaql.core._

class QueryImpl(val c: blackbox.Context) {

  import c.universe._

  def selectAll[T <: DbDataSet] () (implicit tag: WeakTypeTag[T]): Expr[SelectStatement[T]] =
    buildQueryAll[T] ()

  def select[T, R <: DbDataSet] (query: Tree) (implicit tag1: WeakTypeTag[T], tag2: WeakTypeTag[R]): Expr[SelectStatement[R]] =
    buildQuery[T, R] (query)

  def selectFrom[T <: DbDataSet, R <: DbDataSet] (fromQueries: Tree, query: Tree)
                (implicit tag1: WeakTypeTag[T], tag2: WeakTypeTag[R]) : Expr[SelectStatement[R]] =
    buildQuery[T, R] (query)

  def updateAll[T <: DbTable] (setMap: Tree) (implicit tag: WeakTypeTag[T]): Expr[UpdateStatement[T]] =
    buildUpdate[T] (setMap)

  def update[T <: DbTable] (setMap: Tree) (implicit tag: WeakTypeTag[T]): Expr[UpdateStatement[T]] =
    buildUpdate[T] (setMap)

  def deleteAll[T <: DbTable] () (implicit tag: WeakTypeTag[T]): Expr[DeleteStatement[T]]  =
    buildDelete[T] ()

  def delete[T <: DbTable] (where: Tree)  (implicit tag: WeakTypeTag[T]): Expr[DeleteStatement[T]]  =
    buildDelete[T] (Some(where))

  private val extractor = new QueryExtractor[c.type](c)

  private def buildQuery[T, R <: DbDataSet] (queryTree: Tree)
                                            (implicit tag1: WeakTypeTag[T], tag2: WeakTypeTag[R]) = {

    val (clause, params, table) = extractor.getQueryClause(queryTree, weakTypeOf[T].toString, weakTypeOf[R].toString, getParamNames[R])

    emitMessage("Query", clause)

    val subQueries = c.Expr[Seq[SelectStatement[_]]] (Select(c.prefix.tree, TermName("subQueries")))


    c.Expr[SelectStatement[R]](q"""SelectStatement(sqlTemplate = ${clause.sql},
                                                   params = ${params.asInstanceOf[Map[String, Tree]]},
                                                   tableNames = ${table.map(_.sql)},
                                                   subQueries = $subQueries.asInstanceOf[Seq[SelectStatement[DbDataSet]]])""")
  }

  private def buildQueryAll[T <: DbDataSet] () (implicit tag: WeakTypeTag[T]) = {

    val (clause, table) = extractor.getQueryClause(weakTypeOf[T].toString)

    c.Expr[SelectStatement[T]](q"""SelectStatement(sqlTemplate = ${clause.sql},
                                                   params = Map.empty[String, Any],
                                                   tableNames = ${List(table.sql)},
                                                   subQueries = Seq.empty[SelectStatement[DbDataSet]])""")
  }

  private def buildUpdate[T <: DbTable] (updateTree: Tree)
                         (implicit tag: WeakTypeTag[T]) = {

     val (clause, params) = extractor.getUpdateClause(updateTree, weakTypeOf[T].toString)

    emitMessage("Update", clause)

    c.Expr[UpdateStatement[T]](q"""UpdateStatement(sqlTemplate = ${clause.sql},
                                                   params = ${params.asInstanceOf[Map[String, Tree]]})""")
  }

  private def buildDelete[T <: DbTable] (whereTree: Option[Tree] = None)
                         (implicit tag: WeakTypeTag[T]) = {

     val (clause, params) = extractor.getDeleteClause(whereTree, weakTypeOf[T].toString)

    emitMessage("Delete", clause)

    c.Expr[DeleteStatement[T]](q"""DeleteStatement(sqlTemplate = ${clause.sql},
                                                   params = ${params.asInstanceOf[Map[String, Tree]]})""")
  }

  private def emitMessage(operation: String, clause: SQLClause) = {

    val compilationMessage = s""" |Debugging ${operation.toLowerCase()}:\n\n\n${clause.sql}\n\n
                                  |$operation Tree:\n${clause}\n\n\n""".stripMargin

      c.info(c.enclosingPosition, compilationMessage, false)
  }

  private def getParamNames[T: WeakTypeTag]() = weakTypeOf[T].decls
                                                             .filter(_.name.decoded == "<init>")
                                                             .head.asMethod.paramLists.head
                                                             .map(_.name.decoded)
}
