package com.albertprz.logograph

import com.albertprz.logograph.utils.Error.SQLError
import com.albertprz.logograph.core.{SQLExpressionClause, Operator}
import com.albertprz.logograph.config.LogographConfig


trait SQLStatement:

  def sql: String
  def paramList: List[?]
  def run [F[+_]] () (using LogographContext[F]): F[Any]

trait SQLStatefulStatement extends SQLStatement


private object SQLStatement:

  def getParamList (params: Map[String, Any]) =
    (params.values flatMap { _ match {
        case list: List[?] => list
        case other: Any => List(other)
      }
    }).toList


  def getSQL (sqlTemplate: String, params: Map[String, Any]) (using LogographConfig) =
    params.values
          .filter (_.isInstanceOf[List[Any]])
          .foldLeft (sqlTemplate) {
            case (acc, curr: List[?]) => acc.replaceFirst(s"${Operator("in").sql} [?]",
                                                            curr.map(x => "?")
                                                                  .mkString(s"${Operator("in").sql} (", ", ", ")"))
          }
