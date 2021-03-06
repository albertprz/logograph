package ast

import orm._
import scala.reflect.macros.blackbox

class QueryImpl(val c: blackbox.Context) {

  import c.universe._

  def select[T: WeakTypeTag, R: WeakTypeTag] (queryFnTree: Tree) =
    buildFullQuery[T, R] (queryFnTree)

  def selectDebug[T: WeakTypeTag, R: WeakTypeTag] (queryFnTree: Tree) =
    buildFullQuery[T, R] (queryFnTree, debug = true)


  private val extractor = new QueryExtractor[c.type](c)

  private def buildFullQuery[T: WeakTypeTag, R: WeakTypeTag] (queryFnTree: Tree, debug: Boolean = false) = {

    val (queryClause, params) = extractor.getQueryClause(queryFnTree, weakTypeOf[T].toString)


    if (debug) {
      throw new Exception(s""" |  Debugging query: \n\n\n${queryClause.sql}\n\n
                               |Query Tree: \n ${queryClause}\n\n\n""".stripMargin)
    }

    c.Expr[SelectQuery[T, R]](q"""SelectQuery(queryClauseSql = Some(${queryClause.sql}),
                                              params = ${params.asInstanceOf[Map[String, Tree]]})""")
  }
}
