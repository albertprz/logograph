package orm

sealed abstract class DbDataSet extends Product with Serializable
abstract class DbResult extends DbDataSet
abstract class DbTable extends DbDataSet
