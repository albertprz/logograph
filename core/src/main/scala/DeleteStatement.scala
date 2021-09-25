package com.albertprz.maglor

import com.albertprz.maglor.config.MaglorConfig
import com.albertprz.maglor.utils
import utils.StringUtils.*

case class DeleteStatement [T <: DbTable]  (sqlTemplate: String,
                                            params: Map[String, Any] = Map.empty)
                                           extends SQLStatefulStatement:

  lazy val (sql, paramList) = DeleteStatement.generate(this)

  lazy val validate = {}

  def run [F[+_]] () (using context: MaglorContext[F]) =
    context.run(this)

  override def toString () =
    s"Delete Statement: \n\n$sqlTemplate \n\nParams:  \n\n${pprint(params)}\n\n"


object DeleteStatement:

  import SQLStatement.*

  given MaglorConfig = MaglorConfig.get

  def generate [T <: DbTable] (delete: DeleteStatement[T]) =

    val sql = getSQL(delete.sqlTemplate, delete.params)
    val paramList = getParamList(delete.params)

    (sql, paramList)
