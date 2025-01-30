package com.albertprz.logograph.utils

import scala.reflect.runtime.universe._

object ReflectionUtils {

  def extractTypeInfo[T: WeakTypeTag]() = {

    val ctorParamList =
      weakTypeOf[T].decls
        .filter(_.name.decoded == "<init>")
        .head
        .asMethod
        .paramLists
        .head

    val elemNames = ctorParamList.map(_.name.decoded)
    val elemTypes = ctorParamList.map(_.info.toString())
    val fullName = weakTypeOf[T].toString()
    val name = fullName.split('.').last

    TypeInfo(fullName, name, elemNames, elemTypes)
  }
}
