package com.albertprz.logograph.macros

import com.albertprz.logograph.*
import com.albertprz.logograph.core.*
import com.albertprz.logograph.utils.StringUtils.*
import com.albertprz.logograph.utils.TypeInfo

import scala.quoted.*

object QueryImpl:

  def selectAll[T <: DbDataSet] (using Quotes, Type[T]): Expr[SelectStatement[T]] =
    buildQueryAll[T]


  def select[T, R <: DbDataSet] (query: Expr[T => Query[R]], subQueries: Expr[Seq[SelectStatement[?]]])
                                (using Quotes, Type[T], Type[R]): Expr[SelectStatement[R]] =
    buildQuery[T, R] (query, subQueries)


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


  def insert[T <: DbTable] (data: Expr[T]) (using Quotes, Type[T]): Expr[InsertStatement[T]] =

    val typeInfo = extractTypeInfo[T]
    '{ InsertStatement (Left (List ($data)), ${Expr(typeInfo)}) }


  def insertSeq[T <: DbTable] (data: Expr[Seq[T]]) (using Quotes, Type[T]): Expr[InsertStatement[T]] =

    val typeInfo = extractTypeInfo[T]
    '{ InsertStatement (Left ($data), ${Expr(typeInfo)}) }


  def insertQuery[T <: DbTable] (query: Expr[SelectStatement[T]]) (using Quotes, Type[T]): Expr[InsertStatement[T]] =

    val typeInfo = extractTypeInfo[T]
    '{ InsertStatement (Right ($query), ${Expr(typeInfo)}) }

end QueryImpl



given ToExpr[TypeInfo] with
  def apply(t: TypeInfo) (using Quotes) =
    '{ utils.TypeInfo(${Expr(t.fullClassName)}, ${Expr(t.className)}, ${Expr(t.elemNames)}, ${Expr(t.elemTypes)}) }


private def buildQuery[T, R <: DbDataSet] (queryTree: Expr[T => Query[R]], subQueries: Expr[Seq[SelectStatement[?]]])
                                          (using Quotes, Type[T], Type[R]): Expr[SelectStatement[R]] =

  val extractor = new QueryExtractor(quotes)

  import extractor.*
  import quotes.reflect.*

  import extractor.given ToExpr[Any]

  val typeInfo = extractTypeInfo[R]

  val (clause, params, table) = extractor.getQueryClause(queryTree.asTerm, getClassName[T],
                                                         typeInfo.fullClassName, typeInfo.elemNames)

  emitMessage(clause)


  '{ SelectStatement[R](sqlTemplate  = ${Expr(clause.sql)},
                        params       = ${Expr(params.asInstanceOf[Map[String, Any]])},
                        tableNames   = ${Expr(table.map(_.sql))},
                        typeInfo     = ${Expr(typeInfo)},
                        subQueries   = $subQueries.asInstanceOf[Seq[SelectStatement[DbDataSet]]],
                        index        = 0,
                        dependencies = Seq.empty) }


private def buildQueryAll[T <: DbDataSet] (using Quotes, Type[T]): Expr[SelectStatement[T]] =

  val extractor = new QueryExtractor(quotes)

  import extractor.*
  import extractor.given ToExpr[Any]

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
  import extractor.given ToExpr[Any]

  import quotes.reflect.*

  val typeInfo = extractTypeInfo[T]

  val (clause, params) = extractor.getUpdateClause(updateTree.asTerm, typeInfo.fullClassName)

  emitMessage(clause)

  '{ UpdateStatement[T](sqlTemplate = ${Expr(clause.sql)},
                        params      = ${Expr(params.asInstanceOf[Map[String, Any]])}) }


private def buildDelete[T <: DbTable] (whereTree: Option[Expr[T => Where]] = None)
                                      (using Quotes, Type[T]): Expr[DeleteStatement[T]] =

  val extractor = new QueryExtractor(quotes)

  import extractor.*
  import extractor.given ToExpr[Any]

  import quotes.reflect.*


  val typeInfo = extractTypeInfo[T]

  val (clause, params) = extractor.getDeleteClause(whereTree.map(_.asTerm), typeInfo.fullClassName)

  emitMessage(clause)

  '{ DeleteStatement[T](sqlTemplate = ${Expr(clause.sql)},
                        params      = ${Expr(params.asInstanceOf[Map[String, Any]])}) }


private def emitMessage (clause: SQLClause) (using Quotes) =

  import quotes.reflect.*

  val compilationMessage = s"\n${clause.sql}\n\n\n"

  report.info(compilationMessage)


private def getClassName[T: Type] (using Quotes) =

  import quotes.reflect.*

  val className = TypeRepr.of[T].show

  if className.contains("Tuple") then
    className.betweenChars('[', ']')
             .wrapParens()

  else
    className


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


  report.error(s"""Logograph Compilation Error:
                Case classes used in queries must be defined at the top-level within a package scope.
                Case class '${nestedCaseClass.get}' defined within the scope of an object or class is not supported."""
              +"\n\n")


  TypeInfo(fullClassName, className, elemNames, elemTypes)
