package com.albertoperez1994.scalaql

import com.albertoperez1994.scalaql.config.ScalaQLConfig
import com.albertoperez1994.scalaql.utils
import utils.StringUtils._

case class DeleteStatement [T <: DbTable]  (private val sqlTemplate: String,
                                            private val params: Map[String, Any] = Map.empty)
                                           extends SQLStatefulStatement {

  lazy val (sql, paramList) = DeleteStatement.generate(this)

  lazy val validate = {}

  def run [F[+_]] () (implicit context: ScalaQLContext[F]) =
    context.run(this)

  override def toString () =
    s"Delete Statement: \n\n$sqlTemplate \n\nParams:  \n\n${pprint(params)}\n\n"
}

object DeleteStatement {

  import SQLStatement._

  given ScalaQLConfig = ScalaQLConfig.get

  def generate [T <: DbTable] (delete: DeleteStatement[T]) = {

    val sql = getSQL(delete.sqlTemplate, delete.params)
    val paramList = getParamList(delete.params)

    (sql, paramList)
  }
}
