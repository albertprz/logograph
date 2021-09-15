package com.albertoperez1994.scalaql.config

import com.albertoperez1994.scalaql.core
import com.albertoperez1994.scalaql.utils
import utils.{FileUtils, Error, CaseConverter, StringCase}
import core.SQLEngine
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

  import Implicits.given ConfigReader[ScalaQLConfig]

  val configFileName = "scalaql.conf"

  val get = FileUtils.getFile(configFileName)
                          .map(file => ConfigSource.file(file)
                                                    .load[ScalaQLConfig]
                                                    .getOrElse(throw new InvalidScalaQLConfig))
                          .getOrElse(ScalaQLConfig())
}

  object Implicits {

  given ProductHint[ScalaQLConfig] =
    ProductHint[ScalaQLConfig](allowUnknownKeys = false)

  given ConfigReader[CaseConverter] =
    ConfigReader.fromNonEmptyStringTry(StringCase(_).map(CaseConverter.apply).toTry)

  given ConfigReader[SQLEngine] =
    ConfigReader.fromNonEmptyStringTry(SQLEngine(_).toTry)

  given ConfigReader[ScalaQLConfig] =
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
