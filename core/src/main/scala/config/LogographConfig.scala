package com.albertprz.logograph.config

import com.albertprz.logograph.core
import com.albertprz.logograph.utils
import utils.{FileUtils, Error, CaseConverter, StringCase}
import core.SQLEngine
import StringCase.CamelCase
import Error.InvalidLogographConfig

import pureconfig.{ConfigReader, ConfigCursor, ReadsMissingKeys, ConfigSource}
import pureconfig.generic.ProductHint


case class LogographConfig (engine:                Option[SQLEngine]                        = None,
                         tableCaseConverter:    Option[CaseConverter]                    = None,
                         columnCaseConverter:   Option[CaseConverter]                    = None,
                         operatorCaseConverter: Option[CaseConverter]                    = None,
                         tableConverter:        Option[Map[String, String]]              = None,
                         columnConverter:       Option[Map[String, Map[String, String]]] = None,
                         operatorConverter:     Option[Map[String, String]]              = None)


object LogographConfig:

  import Implicits.given ConfigReader[LogographConfig]

  val configFileName = "logograph.conf"

  val get = FileUtils.getFile(configFileName)
                          .map(file => ConfigSource.file(file)
                                                    .load[LogographConfig]
                                                    .getOrElse(throw new InvalidLogographConfig))
                          .getOrElse(LogographConfig())

object Implicits:

  given ProductHint[LogographConfig] =
    ProductHint[LogographConfig](allowUnknownKeys = false)

  given ConfigReader[CaseConverter] =
    ConfigReader.fromNonEmptyStringTry(StringCase(_).map(CaseConverter.apply).toTry)

  given ConfigReader[SQLEngine] =
    ConfigReader.fromNonEmptyStringTry(SQLEngine(_).toTry)

  given ConfigReader[LogographConfig] =
    ConfigReader.forProduct7[LogographConfig, Option[SQLEngine],
                             Option[CaseConverter], Option[CaseConverter], Option[CaseConverter],
                             Option[Map[String, String]], Option[Map[String, Map[String, String]]],
                             Option[Map[String, String]]] (
      "engine", "tableCaseConverter", "columnCaseConverter", "operatorCaseConverter",
                "tableConverter", "columnConverter", "operatorConverter") {
      case (engine, tableCaseConverter, columnCaseConverter, operatorCaseConverter,
                    tableConverter, columnConverter, operatorConverter) =>
        LogographConfig(engine, tableCaseConverter, columnCaseConverter, operatorCaseConverter,
                    tableConverter, columnConverter, operatorConverter)
    }
