package utils

object ReflectionUtils {

  import scala.reflect.runtime.universe._
  private lazy val universeMirror = runtimeMirror(getClass.getClassLoader)

  def companionOf[T: TypeTag] = {
    val companionMirror = universeMirror.reflectModule(typeOf[T].typeSymbol.companionSymbol.asModule)
    Companion(companionMirror.instance)
  }

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

    val paramCount =
      applyMethod.getParameterCount

    val className =
      companionObject.getClass.getSimpleName.replace("$", "")
  }
}
