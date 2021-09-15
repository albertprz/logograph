package test

import com.albertoperez1994.scalaql.DbResult


case class Result (name: String, age: Int, street: String, telephoneNumber: Long) extends DbResult
case class InnerResult1 (name: String, age: Int, street: String) extends DbResult
case class InnerResult2 (name: String, telephoneNumber: Long) extends DbResult
