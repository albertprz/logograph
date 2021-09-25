package com.albertprz.maglor

sealed trait DbDataSet extends Product with Serializable
trait DbResult extends DbDataSet
trait DbTable extends DbDataSet
