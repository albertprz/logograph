package com.albertprz.logograph.utils

import scala.quoted.*

case class TypeInfo(
    fullClassName: String,
    className: String,
    elemNames: List[String],
    elemTypes: List[String]
)

object TypeInfo:

  def build(tuple: (String, String, List[String], List[String])) =

    val (fullClassName, className, elemNames, elemTypes) = tuple
    TypeInfo(fullClassName, className, elemNames, elemTypes)
