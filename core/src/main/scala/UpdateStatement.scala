package com.albertoperez1994.scalaql

import com.albertoperez1994.scalaql.utils
import utils.StringUtils._

case class UpdateStatement [T <: DbTable] (sqlTemplate: String,
                                           params: Map[String, Any] = Map.empty)
                                          extends SQLStatefulStatement {

  lazy val (sql, paramList) = UpdateStatement.generate(this)

  def run [F[+_]] () (implicit context: ScalaQLContext[F]) =
    context.run(this)

  override def toString () =
    s"Update Statement: \n\n$sqlTemplate \n\nParams:  \n\n${pprint(params)}\n\n"
}

private object UpdateStatement {

  import SQLStatement._

  def generate [T <: DbTable] (update: UpdateStatement[T]) = {

    val sql = getSQL(update.sqlTemplate, update.params)
    val paramList = getParamList(update.params)

    (sql, paramList)
  }
}
