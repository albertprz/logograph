package com.albertoperez1994.scalaql.core

import com.albertoperez1994.scalaql.utils.StringUtils._
import com.albertoperez1994.scalaql.utils.Error.CaseNotAllowed


enum SQLEngine:
  case PostgreSQL, SQLServer, Oracle, SQLite, MySQL

object SQLEngine:

  def apply(engineName: String) =
    engineName.normalizedToLower() match
      case "postgresql" => Right(PostgreSQL)
      case "sqlserver"  => Right(SQLServer)
      case "oracle"     => Right(Oracle)
      case "sqlite"     => Right(SQLite)
      case "mysql"      => Right(MySQL)
      case _            => Left(CaseNotAllowed(engineName, SQLEngine.toString(), SQLEngine.values.toSet))
