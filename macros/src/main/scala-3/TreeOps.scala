// package com.albertprz.logograph.macros

// import scala.quoted.*


// class TreeOps (using val quotes: Quotes) {

//   import quotes.reflect.*


//   def findMapArgs (tree: Tree) = {

//     val mapEntries = findAll(tree) (t => t.asExpr match {
//         case '{ scala.Predef.ArrowAssoc($key: t1).->($value: t2) } => true
//         case _                                                     => false
//     } )

//     for mapEntry <- mapEntries
//       yield mapEntry.asExpr match {
//         case '{ scala.Predef.ArrowAssoc($key: t1).->($value: t2) } => (key, value)
//       }
//   }

//   def findTypedCtorArgs (tree: Tree, className: String) = {

//     val ctor = findAll(tree) (t => t match {
//         case Apply(Apply(TypeApply(Select(x, _), _), _), _) => x.toString.contains(s".$className")
//         case Apply(TypeApply(Select(x, _), _), _)           => x.toString.contains(s".$className")
//         case _ => false
//       })

//     val ctorArgs = ctor flatMap extractFnArgs

//     val args = for  argList <- ctorArgs
//                     arg <- argList
//       yield arg match {
//           case Apply(_, args) => Some(args)
//           case _ => None
//       }

//     args.flatten
//   }

//   def findCtorArgs (tree: Tree, className: String) = {

//     val ctor = findAll(tree) (t => t match {
//         case Apply(Apply(TypeApply(Select(x, _), _), _), _) => x.toString.contains(s".$className")
//         case Apply(TypeApply(Select(x, _), _), _)           => x.toString.contains(s".$className")
//         case Apply(Apply(Select(x, _), _), _)               => x.toString.contains(s".$className")
//         case Apply(Select(x, _), _)                         => x.toString.contains(s".$className")
//         case _                                              => false
//       })

//     ctor flatMap extractFnArgs
//   }

//   def findLambdaFnArgs (tree: Tree) = {

//     val lambdaFn = findAll(tree) (t => t match {
//         case _: CaseDef | _: ValDef => true
//         case _                      => false
//       })

//     (lambdaFn flatMap extractCaseDefArgs) ++
//     (lambdaFn flatMap extractValDefArgs)
//   }


//   private def extractCaseDefArgs (tree: Tree) = {

//     val args = tree match {
//       case CaseDef(Apply(_, args), _, _) => args
//       case CaseDef(arg @ Bind(_, _), _, _) => List(arg)
//       case _ => List.empty
//     }

//     (for arg <- args
//       yield arg match {
//         case Bind(arg, _) => Some(arg)
//         case _ => None
//       }
//     ).flatten
//   }

//   private def extractValDefArgs (tree: Tree) =
//     tree match {
//       case ValDef(arg, _ , _) => List(arg)
//       case _ => List.empty
//     }

//   private def extractFnArgs (tree: Tree) =
//     tree match {
//       case Apply(Apply(_, args1), args2) => List(args1, args2)
//       case Apply(_, args) => List(args)
//       case _ => List.empty
//     }

//   private def findAll (tree: Tree)(filterFn: Tree => Boolean): List[Tree] = {

//     val acc = new TreeAccumulator[List[Tree]] {
//       def foldTree(matchedTrees: List[Tree], tree: Tree)(owner: Symbol): List[Tree] = {

//         val current =
//           if filterFn(tree) then  List (tree)
//           else                    Nil

//         foldOverTree(matchedTrees ++ current, tree)(owner)
//       }
//     }

//     acc.foldOverTree(Nil, tree) (tree.symbol)
//   }
// }
