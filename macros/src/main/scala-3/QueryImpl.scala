package com.albertoperez1994.scalaql.macros

import com.albertoperez1994.scalaql.*
import com.albertoperez1994.scalaql.core.*
import com.albertoperez1994.scalaql.utils.TypeInfo

import scala.quoted.*

object QueryImpl:

  def selectAll[T <: DbDataSet] (using Quotes, Type[T]): Expr[SelectStatement[T]] =
    buildQueryAll[T]


  def select[T, R <: DbDataSet] (query: Expr[T => Query[R]])
                                (using Quotes, Type[T], Type[R]): Expr[SelectStatement[R]] =
    buildQuery[T, R] (query)


  def updateAll[T <: DbTable] (setMap: Expr[T => Map[Any, Any]])
                              (using Quotes, Type[T]): Expr[UpdateStatement[T]] =
    buildUpdate[T] (setMap)


  def update[T <: DbTable] (setMap: Expr[T => (Map[Any, Any], Where)])
                          (using Quotes, Type[T]): Expr[UpdateStatement[T]] =
    buildUpdate[T] (setMap)


  def deleteAll[T <: DbTable] (using Quotes, Type[T]): Expr[DeleteStatement[T]]  =
    buildDelete[T] (None)


  def delete[T <: DbTable] (where: Expr[T => Where])
                          (using Quotes, Type[T]): Expr[DeleteStatement[T]]  =
    buildDelete[T] (Some(where))


given ToExpr[Expr[Any]] with
  def apply(exp: Expr[Any]) (using Quotes) = Expr(exp)


given ToExpr[TypeInfo] with
  def apply(t: TypeInfo) (using Quotes) =
    '{ utils.TypeInfo(${Expr(t.fullClassName)}, ${Expr(t.className)}, ${Expr(t.elemNames)}, ${Expr(t.elemTypes)}) }


private def buildQuery[T, R <: DbDataSet] (queryTree: Expr[T => Query[R]])
                                          (using Quotes, Type[T], Type[R]): Expr[SelectStatement[R]] =

  val extractor = new QueryExtractor(quotes)

  import extractor.*
  import quotes.reflect.*

  val (typeInfoT, typeInfoR) = (extractTypeInfo[T], extractTypeInfo[R])
  val (clause, params, table) = extractor.getQueryClause(queryTree.asTerm, typeInfoT.fullClassName,
                                                         typeInfoR.fullClassName, typeInfoR.elemNames)

  emitMessage("Query", clause)


  '{ SelectStatement[R](sqlTemplate  = ${Expr(clause.sql)},
                        params       = Map.empty,
                        // params       = ${Expr(params.asInstanceOf[Map[String, Expr[Any]]])},
                        tableNames   = ${Expr(table.map(_.sql))},
                        typeInfo     = ${Expr(typeInfoR)},
                        subQueries   = Seq.empty,
                        index        = 0,
                        dependencies = Seq.empty) }


private def buildQueryAll[T <: DbDataSet] (using Quotes, Type[T]): Expr[SelectStatement[T]] =

  val extractor = new QueryExtractor(quotes)

  import extractor.*
  import quotes.reflect.*

  val typeInfo = extractTypeInfo[T]

  val (clause, table) = extractor.getQueryClause(typeInfo.fullClassName)

  '{ SelectStatement[T](sqlTemplate  = ${Expr(clause.sql)},
                        params       = Map.empty,
                        tableNames   = ${Expr(List(table.sql))},
                        typeInfo     = ${Expr(typeInfo)},
                        subQueries   = Seq.empty,
                        index        = 0,
                        dependencies = Seq.empty) }


private def buildUpdate[T <: DbTable] (updateTree: Expr[Any])
                                      (using Quotes, Type[T]): Expr[UpdateStatement[T]] =

  val extractor = new QueryExtractor(quotes)

  import extractor.*
  import quotes.reflect.*

  val typeInfo = extractTypeInfo[T]

  val (clause, params) = extractor.getUpdateClause(updateTree.asTerm, typeInfo.fullClassName)

  emitMessage("Update", clause)

  '{ UpdateStatement[T](sqlTemplate = ${Expr(clause.sql)},
                        params      = ${Expr(params.asInstanceOf[Map[String, Expr[Any]]])}) }


private def buildDelete[T <: DbTable] (whereTree: Option[Expr[T => Where]] = None)
                                      (using Quotes, Type[T]): Expr[DeleteStatement[T]] =

  val extractor = new QueryExtractor(quotes)

  import extractor.*
  import quotes.reflect.*

  val typeInfo = extractTypeInfo[T]

  val (clause, params) = extractor.getDeleteClause(whereTree.map(_.asTerm), typeInfo.fullClassName)

  emitMessage("Delete", clause)

  '{ DeleteStatement[T](sqlTemplate = ${Expr(clause.sql)},
                        params      = ${Expr(params.asInstanceOf[Map[String, Expr[Any]]])}) }


private def emitMessage (operation: String, clause: SQLClause) (using Quotes) =

  import quotes.reflect.*

  val compilationMessage = s"Debugging ${operation.toLowerCase()}:\n\n\n${clause.sql}\n\n\n"

  report.info(compilationMessage)


private def extractTypeInfo[T: Type] (using Quotes) =

  import quotes.reflect.*


  val typeSymbol = TypeRepr.of[T].typeSymbol
  val className = typeSymbol.name
  val fullClassName = typeSymbol.fullName
  val fields = typeSymbol.caseFields

  val elemNames = fields.map(_.name)
  val elemTypes = fields.map(_.tree match { case v: ValDef => v.tpt.tpe.typeSymbol.name })


  val nestedCaseClass = fullClassName.split(',')
                                     .find(_.split('.').dropRight(1).last.head.isUpper)

  if nestedCaseClass.nonEmpty then


  report.error(s"""ScalaQL Compilation Error:
                Case classes used in queries must be defined at the top-level within a package scope.
                Case class '${nestedCaseClass.get}' defined within the scope of an object or class is not supported."""
              +"\n\n")


  TypeInfo(fullClassName, className, elemNames, elemTypes)
