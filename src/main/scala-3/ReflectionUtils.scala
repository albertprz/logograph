package com.albertoperez1994.scalaql.utils

import scala.quoted.*


object ReflectionUtils:

  def extractTypeInfo[T: Type] (using Quotes): TypeInfo =

    import quotes.reflect.*

    val typeSymbol = TypeRepr.of[T].typeSymbol
    val className = typeSymbol.name
    val fullClassName = typeSymbol.fullName
    val fields = typeSymbol.caseFields

    val elemNames = fields.map(_.name)
    val elemTypes = fields.map(_.tree match { case v: ValDef => v.tpt.tpe.typeSymbol.name })


    TypeInfo(fullClassName, className, elemNames, elemTypes)
