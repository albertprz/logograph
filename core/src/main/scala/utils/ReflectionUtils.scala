package com.albertoperez1994.scalaql.utils

import scala.reflect.runtime.universe._

object ReflectionUtils {

  private lazy val universeMirror = runtimeMirror(getClass.getClassLoader)

  def constructorOf[T: TypeTag] = {

    val classSymbol = weakTypeOf[T].typeSymbol.asClass
    val classConstructor = classSymbol.info.member(termNames.CONSTRUCTOR).asMethod
    val classMirror = universeMirror.reflectClass(classSymbol)
    Constructor(classMirror.reflectConstructor(classConstructor))
  }

  def className[T: TypeTag] =
    weakTypeOf[T].toString.split('.').last

  case class Constructor(constructor: MethodMirror) {

    def apply (x: Seq[Any]) = constructor.apply(x :_ *)

    val paramNames = constructor.symbol
                                .paramLists
                                .head
                                .map(_.name.decoded)

    val paramTypes = constructor.symbol
                                .paramLists
                                .head
                                .map(_.typeSignature.typeSymbol.fullName)
  }
}
