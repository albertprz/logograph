package com.albertprz.maglor.utils


object Error:

  case class CaseNotAllowed (elem: String, elemType: String, set: Set[?])
      extends Exception(s"Element $elem does not belong to the set of allowed $elemType: $set")

  case class InvalidMaglorConfig ()
      extends Exception("Maglor config is not valid")
