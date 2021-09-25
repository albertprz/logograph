package com.albertprz.maglor

import com.albertprz.maglor.config.MaglorConfig
import com.albertprz.maglor.utils
import utils.StringUtils.*

case class UpdateStatement [T <: DbTable] (sqlTemplate: String,
                                           params: Map[String, Any] = Map.empty)
                                          extends SQLStatefulStatement:

  lazy val (sql, paramList) = UpdateStatement.generate(this)

  lazy val validate = {}

  def run [F[+_]] () (using context: MaglorContext[F]) =
    context.run(this)

  override def toString () =
    s"Update Statement: \n\n$sqlTemplate \n\nParams:  \n\n${pprint(params)}\n\n"


object UpdateStatement:

  import SQLStatement.*

  given MaglorConfig = MaglorConfig.get

  def generate [T <: DbTable] (update: UpdateStatement[T]) =

    val sql = getSQL(update.sqlTemplate, update.params)
    val paramList = getParamList(update.params)

    (sql, paramList)
