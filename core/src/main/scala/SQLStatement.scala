package com.albertoperez1994.scalaql

import com.albertoperez1994.scalaql.core.{SQLClause, Operator}
import com.albertoperez1994.scalaql.config.ScalaQLConfig

trait SQLStatement {

  def validate: Unit
  def sql: String
  def paramList: List[Any]
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


  def getSQL (sqlTemplate: String, params: Map[String, Any]) (implicit cfg: ScalaQLConfig) =
    params.values
          .filter (_.isInstanceOf[List[Any]])
          .foldLeft (sqlTemplate) {
            case (acc, curr: List[Any]) => acc.replaceFirst(s"${Operator("in").sql} [?]",
                                                            curr.map(x => "?")
                                                                  .mkString(s"${Operator("in").sql} (", ", ", ")"))
          }
}
