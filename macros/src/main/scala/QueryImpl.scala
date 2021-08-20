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

  def selectDebug[T, R <: DbDataSet] (query: Tree) (implicit tag1: WeakTypeTag[T], tag2: WeakTypeTag[R]): Expr[SelectStatement[R]] =
    buildQuery[T, R] (query, debug = true)

  def updateAll[T <: DbTable] (setMap: Tree) (implicit tag: WeakTypeTag[T]): Expr[UpdateStatement[T]] =
    buildUpdate[T] (setMap)

  def update[T <: DbTable] (setMap: Tree) (implicit tag: WeakTypeTag[T]): Expr[UpdateStatement[T]] =
    buildUpdate[T] (setMap)

  def updateDebug[T <: DbTable] (setMap: Tree) (implicit tag: WeakTypeTag[T]): Expr[UpdateStatement[T]] =
    buildUpdate[T] (setMap, debug = true)

  def deleteAll[T <: DbTable] (implicit tag: WeakTypeTag[T]): Expr[DeleteStatement[T]]  =
    buildDelete[T] ()

  def delete[T <: DbTable] (where: Tree)  (implicit tag: WeakTypeTag[T]): Expr[DeleteStatement[T]]  =
    buildDelete[T] (Some(where))

  def deleteDebug[T <: DbTable] (where: Tree)  (implicit tag: WeakTypeTag[T]): Expr[DeleteStatement[T]]  =
    buildDelete[T] (Some(where), debug = true)

  private val extractor = new QueryExtractor[c.type](c)

  private def buildQuery[T, R <: DbDataSet] (queryTree: Tree, debug: Boolean = false)
                                            (implicit tag1: WeakTypeTag[T], tag2: WeakTypeTag[R]) = {

    val (clause, params) = extractor.getQueryClause(queryTree, weakTypeOf[T].toString)

    emitMessage("Query", clause, debug)

    c.Expr[SelectStatement[R]](q"""SelectStatement(sqlTemplate = ${clause.sql},
                                                   params = ${params.asInstanceOf[Map[String, Tree]]})""")
  }

  private def buildQueryAll[T <: DbDataSet] () (implicit tag: WeakTypeTag[T]) = {

    val clause = extractor.getQueryClause(weakTypeOf[T].toString)

    c.Expr[SelectStatement[T]](q"""SelectStatement(sqlTemplate = ${clause.sql},
                                                   params = Map.empty[String, Any])""")
  }

  private def buildUpdate[T <: DbTable] (updateTree: Tree, debug: Boolean = false)
                         (implicit tag: WeakTypeTag[T]) = {

     val (clause, params) = extractor.getUpdateClause(updateTree, weakTypeOf[T].toString)

    emitMessage("Update", clause, debug)

    c.Expr[UpdateStatement[T]](q"""UpdateStatement(sqlTemplate = ${clause.sql},
                                                   params = ${params.asInstanceOf[Map[String, Tree]]})""")
  }

  private def buildDelete[T <: DbTable] (whereTree: Option[Tree] = None, debug: Boolean = false)
                         (implicit tag: WeakTypeTag[T]) = {

     val (clause, params) = extractor.getDeleteClause(whereTree, weakTypeOf[T].toString)

    emitMessage("Delete", clause, debug)

    c.Expr[DeleteStatement[T]](q"""DeleteStatement(sqlTemplate = ${clause.sql},
                                                   params = ${params.asInstanceOf[Map[String, Tree]]})""")
  }

  private def emitMessage(operation: String, clause: SQLClause, debug: Boolean) = {

    val compilationMessage = s""" |  Debugging ${operation.toLowerCase()}: \n\n\n${clause.sql}\n\n
                                  |$operation Tree: \n ${clause}\n\n\n""".stripMargin

      c.info(c.enclosingPosition, compilationMessage, debug)
  }
}
