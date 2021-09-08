package it

import com.albertoperez1994.scalaql.DbResult

object TestResultModels {

  case class Result (name: String, age: Int, street: String, telephoneNumber: Long) extends DbResult
}
