package com.albertoperez1994.scalaql.utils

import scala.quoted.*

case class TypeInfo (fullClassName: String, className: String, elemNames: List[String], elemTypes: List[String])


object TypeInfo {

  def build(tuple: (String, String, List[String], List[String])) = {

    val (fullClassName, className, elemNames, elemTypes) = tuple
    TypeInfo(fullClassName, className, elemNames, elemTypes)
  }

  inline def apply[T]: (String, String, List[String], List[String]) = ${ typeInfoImpl[T] }

  def typeInfoImpl[T: Type](using q: Quotes): Expr[(String, String, List[String], List[String])] = {

    import q.reflect.*

    val typeSymbol = TypeRepr.of[T].typeSymbol
    val className = typeSymbol.name
    val fullClassName = typeSymbol.fullName
    val fields = typeSymbol.caseFields

    val elemNames = fields.map(_.name)
    val elemTypes = fields.map(_.tree match { case v: ValDef => v.tpt.tpe.typeSymbol.name })


    Expr((fullClassName, className, elemNames, elemTypes))
  }
}


// object ReflectionUtils {


//   def apply[T <: Product] (xs: Seq[Any]) (using mirror: Mirror.ProductOf[T]) = {

//     val tuple = xs.foldRight[Tuple](EmptyTuple)(_ *: _)
//     mirror.fromProduct(tuple)
//   }

//   def elemNames[T <: Product] () (using mirror: Mirror.ProductOf[T], elemLabels: mirror.MirroredElemLabels) =
//     elemLabels.productElementNames.toSeq

//   def elemTypes[T] () (using mirror: Mirror.ProductOf[T], types: mirror.MirroredElemTypes) =
//     types.productElementNames.toSeq

//   def className[T] () (using mirror: Mirror.ProductOf[T], label: mirror.MirroredLabel) =
//     label.toString()
// }
