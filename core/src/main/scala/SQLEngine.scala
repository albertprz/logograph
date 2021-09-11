package com.albertoperez1994.scalaql.core

import com.albertoperez1994.scalaql.utils.StringUtils._
import com.albertoperez1994.scalaql.utils.Error.CaseNotAllowed

sealed trait SQLEngine

object SQLEngine {

  val engines = Set(PostgreSQL, SQLServer, Oracle, SQLite, MySQL)
                  .map(_.productPrefix)

  def apply(engineName: String) = {
    engineName.normalizedToLower() match {
      case "postgresql" => Right(PostgreSQL)
      case "sqlserver"  => Right(SQLServer)
      case "oracle"     => Right(Oracle)
      case "sqlite"     => Right(SQLite)
      case "mysql"      => Right(MySQL)
      case _            => Left(CaseNotAllowed(engineName, SQLEngine.toString(), engines))
    }
  }

  case object PostgreSQL extends SQLEngine
  case object SQLServer  extends SQLEngine
  case object Oracle     extends SQLEngine
  case object SQLite     extends SQLEngine
  case object MySQL      extends SQLEngine
}
