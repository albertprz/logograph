package com.albertoperez1994.scalaql.core

sealed trait DbDataSet extends Product with Serializable {

  val * = this
}
trait DbResult extends DbDataSet
trait DbTable extends DbDataSet
