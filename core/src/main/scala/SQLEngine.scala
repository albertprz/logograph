package com.albertprz.maglor.core

import com.albertprz.maglor.utils.StringUtils._
import com.albertprz.maglor.utils.Error.CaseNotAllowed


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
