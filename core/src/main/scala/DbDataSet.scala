package com.albertoperez1994.scalaql

import com.albertoperez1994.scalaql.utils.StringUtils.CaseConverter

sealed trait DbDataSet extends Product with Serializable
trait DbResult extends DbDataSet
trait DbTable extends DbDataSet

case class DbConfig (tableConverter: Option[CaseConverter[_, _]] = None,
                     columnConverter: Option[CaseConverter[_, _]] = None,
                     operatorConverter: Option[CaseConverter[_, _]] = None)
