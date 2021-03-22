package com.albertoperez1994.scalaql.utils

object ReflectionUtils {

  import scala.reflect.runtime.universe._
  private lazy val universeMirror = runtimeMirror(getClass.getClassLoader)

  def companionOf[T: TypeTag] = {

    val module = typeOf[T].typeSymbol.companionSymbol.asModule
    val companionMirror = universeMirror.reflectModule(module)
    Companion(companionMirror.instance)
  }

  def className[T: TypeTag] =
    typeOf[T].toString.split('.').last

  case class Companion(companionObject: Any) {

    def apply (x: Seq[Any]) =
      applyMethod.invoke(companionObject, x :_ *)

    private val applyMethod =
      companionObject.getClass.getDeclaredMethods
                  .filter(_.getName == "apply")
                  .find(_.getReturnType.getName != "java.lang.Object").get

    val paramNames =
      applyMethod.getParameters
                    .map(_.getName).toList

    val paramTypes =
      applyMethod.getParameterTypes
                    .map(_.getName).toList
  }
}
