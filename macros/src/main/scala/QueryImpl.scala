package ast

import orm._
import scala.reflect.macros.blackbox

class QueryImpl(val c: blackbox.Context) {

  import c.universe._

  def select[T, R <: DbDataSet] (query: Tree) (implicit tag1: WeakTypeTag[T], tag2: WeakTypeTag[R]) =
    buildQuery[T, R] (query)

  def selectDebug[T, R <: DbDataSet] (query: Tree) (implicit tag1: WeakTypeTag[T], tag2: WeakTypeTag[R]) =
    buildQuery[T, R] (query, debug = true)

  def update[T <: DbTable] (setMap: Tree) (implicit tag: WeakTypeTag[T]) =
    buildUpdate[T] (setMap)

  def updateWhere[T <: DbTable] (setMap: Tree) (implicit tag: WeakTypeTag[T]) =
    buildUpdate[T] (setMap)

  def delete[T <: DbTable] (implicit tag: WeakTypeTag[T])  =
    buildDelete[T] ()

  def deleteWhere[T <: DbTable] (where: Tree)  (implicit tag: WeakTypeTag[T])  =
    buildDelete[T] (Some(where))

  private val extractor = new QueryExtractor[c.type](c)

  private def buildQuery[T, R <: DbDataSet] (queryTree: Tree, debug: Boolean = false)
                                            (implicit tag1: WeakTypeTag[T], tag2: WeakTypeTag[R]) = {

    val (clause, params) = extractor.getQueryClause(queryTree, weakTypeOf[T].toString)

    if (debug) {
      throw new Exception(s""" |  Debugging query: \n\n\n${clause.sql}\n\n
                               |Query Tree: \n ${clause}\n\n\n""".stripMargin)
    }

    c.Expr[SelectStatement[R]](q"""SelectStatement(sqlTemplate = Some(${clause.sql}),
                                                   params = ${params.asInstanceOf[Map[String, Tree]]})""")
  }

  private def buildUpdate[T <: DbTable] (updateTree: Tree) (implicit tag: WeakTypeTag[T]) = {

     val (clause, params) = extractor.getUpdateClause(updateTree, weakTypeOf[T].toString)

    c.Expr[UpdateStatement[T]](q"""UpdateStatement(sqlTemplate = ${clause.sql},
                                                   params = ${params.asInstanceOf[Map[String, Tree]]})""")
  }

  private def buildDelete[T <: DbTable] (whereTree: Option[Tree] = None) (implicit tag: WeakTypeTag[T]) = {

     val (clause, params) = extractor.getDeleteClause(whereTree, weakTypeOf[T].toString)

    c.Expr[DeleteStatement[T]](q"""DeleteStatement(sqlTemplate = ${clause.sql},
                                                   params = ${params.asInstanceOf[Map[String, Tree]]})""")
  }

}
