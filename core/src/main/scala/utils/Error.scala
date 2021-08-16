package com.albertoperez1994.scalaql.utils

object Error {
  case class CaseNotAllowed (elem: String, elemType: String, set: Set[String])
      extends Exception(s"Element $elem does not belong to the set of allowed $elemType: $set")

  case class InvalidScalaQLConfig ()
      extends Exception("ScalaQL config is not valid")
}
