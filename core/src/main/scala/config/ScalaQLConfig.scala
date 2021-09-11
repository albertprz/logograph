package com.albertoperez1994.scalaql.config

import com.albertoperez1994.scalaql.core.SQLEngine
import com.albertoperez1994.scalaql.utils.{FileUtils, Error, CaseConverter, StringCase}
import StringCase.CamelCase
import Error.InvalidScalaQLConfig

import pureconfig.{ConfigReader, ConfigCursor, ReadsMissingKeys, ConfigSource}
import pureconfig.generic.ProductHint


case class ScalaQLConfig (engine:                Option[SQLEngine]                = None,
                          tableCaseConverter:    Option[CaseConverter]            = None,
                          columnCaseConverter:   Option[CaseConverter]            = None,
                          operatorCaseConverter: Option[CaseConverter]            = None,
                          tableConverter:        Map[String, String]              = Map.empty,
                          columnConverter:       Map[String, Map[String, String]] = Map.empty,
                          operatorConverter:     Map[String, String]              = Map.empty)


object ScalaQLConfig {

  import Implicits._

  val configFileName = "scalaql.conf"

  val get = FileUtils.getFile(configFileName)
                     .map(file => ConfigSource.file(file)
                                              .load[ScalaQLConfig]
                                              .getOrElse(throw new InvalidScalaQLConfig))
                     .getOrElse(ScalaQLConfig())
}

private object Implicits {

  implicit val hint: ProductHint[ScalaQLConfig] = ProductHint[ScalaQLConfig](allowUnknownKeys = false)

  implicit val caseConverterConfigReader: ConfigReader[CaseConverter] =
    ConfigReader.fromNonEmptyStringTry(StringCase(_).map(CaseConverter.apply).toTry)

  implicit val engineConfigReader: ConfigReader[SQLEngine] =
    ConfigReader.fromNonEmptyStringTry(SQLEngine(_).toTry)

  implicit def mapConfigReader[T: ConfigReader] = new ConfigReader[Map[String, T]] with ReadsMissingKeys {
    override def from(cur: ConfigCursor) =
      if (cur.isUndefined) Right(Map.empty) else ConfigReader[Map[String, T]].from(cur)
  }

  implicit val configReader: ConfigReader[ScalaQLConfig] =
    ConfigReader.forProduct7[ScalaQLConfig, Option[SQLEngine],
                             Option[CaseConverter], Option[CaseConverter], Option[CaseConverter],
                             Map[String, String], Map[String, Map[String, String]], Map[String, String]] (
      "engine", "tableCaseConverter", "columnCaseConverter", "operatorCaseConverter",
                "tableConverter", "columnConverter", "operatorConverter") {
      case (engine, tableCaseConverter, columnCaseConverter, operatorCaseConverter,
                    tableConverter, columnConverter, operatorConverter) =>
        ScalaQLConfig(engine, tableCaseConverter, columnCaseConverter, operatorCaseConverter,
                    tableConverter, columnConverter, operatorConverter)
    }
}
