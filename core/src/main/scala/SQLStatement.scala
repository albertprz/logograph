package com.albertoperez1994.scalaql

trait SQLStatement {

  val sql: String
  val paramList: List[Any]
  def run [F[+_]] () (implicit context: ScalaQLContext[F]): F[Any]
}

trait SQLStatefulStatement extends SQLStatement

private object SQLStatement {

  def getParamList (params: Map[String, Any]) =
    (params.values flatMap { _ match {
        case list: List[Any] => list
        case other: Any => List(other)
      }
    }).toList


  def getSQL (sqlTemplate: String, params: Map[String, Any]) =
    params.values
          .filter (_.isInstanceOf[List[Any]])
          .foldLeft (sqlTemplate) {
            case (acc, curr: List[Any]) => acc.replaceFirst("in [?]", curr.map(x => "?")
                                                                          .mkString("in (", ", ", ")"))
          }
}
