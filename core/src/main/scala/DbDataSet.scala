package com.albertoperez1994.scalaql

sealed trait DbDataSet extends Product with Serializable
trait DbResult extends DbDataSet
trait DbTable extends DbDataSet
