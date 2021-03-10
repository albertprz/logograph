package ast

import orm._
import scala.reflect.macros.blackbox

class QueryImpl(val c: blackbox.Context) {

  import c.universe._

  def select[T, R <: DbDataSet] (tree: Tree) (implicit tag1: WeakTypeTag[T], tag2: WeakTypeTag[R]) =
    buildQuery[T, R] (tree)

  def selectDebug[T, R <: DbDataSet] (tree: Tree) (implicit tag1: WeakTypeTag[T], tag2: WeakTypeTag[R]) =
    buildQuery[T, R] (tree, debug = true)

  def update[T <: DbTable] (tree: Tree) (implicit tag: WeakTypeTag[T]) =
    buildUpdate[T] (tree)


  private val extractor = new QueryExtractor[c.type](c)

  private def buildQuery[T, R <: DbDataSet] (tree: Tree, debug: Boolean = false)
                                            (implicit tag1: WeakTypeTag[T], tag2: WeakTypeTag[R]) = {

    val (clause, params) = extractor.getQueryClause(tree, weakTypeOf[T].toString)

    if (debug) {
      throw new Exception(s""" |  Debugging query: \n\n\n${clause.sql}\n\n
                               |Query Tree: \n ${clause}\n\n\n""".stripMargin)
    }

    c.Expr[SelectQuery[R]](q"""SelectQuery(sqlTemplate = Some(${clause.sql}),
                                           params = ${params.asInstanceOf[Map[String, Tree]]})""")
  }

  private def buildUpdate[T <: DbTable] (tree: Tree) (implicit tag: WeakTypeTag[T]) = {

     val (clause, params) = extractor.getUpdateClause(tree, weakTypeOf[T].toString)

    c.Expr[UpdateStatement[T]](q"""UpdateStatement(sqlTemplate = ${clause.sql},
                                                   params = ${params.asInstanceOf[Map[String, Tree]]})""")
  }
}
