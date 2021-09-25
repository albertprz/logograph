package com.albertprz.maglor.config

import com.albertprz.maglor.core
import com.albertprz.maglor.utils
import utils.{FileUtils, Error, CaseConverter, StringCase}
import core.SQLEngine
import StringCase.CamelCase
import Error.InvalidMaglorConfig

import pureconfig.{ConfigReader, ConfigCursor, ReadsMissingKeys, ConfigSource}
import pureconfig.generic.ProductHint


case class MaglorConfig (engine:                Option[SQLEngine]                        = None,
                         tableCaseConverter:    Option[CaseConverter]                    = None,
                         columnCaseConverter:   Option[CaseConverter]                    = None,
                         operatorCaseConverter: Option[CaseConverter]                    = None,
                         tableConverter:        Option[Map[String, String]]              = None,
                         columnConverter:       Option[Map[String, Map[String, String]]] = None,
                         operatorConverter:     Option[Map[String, String]]              = None)


object MaglorConfig:

  import Implicits.given ConfigReader[MaglorConfig]

  val configFileName = "maglor.conf"

  val get = FileUtils.getFile(configFileName)
                          .map(file => ConfigSource.file(file)
                                                    .load[MaglorConfig]
                                                    .getOrElse(throw new InvalidMaglorConfig))
                          .getOrElse(MaglorConfig())

object Implicits:

  given ProductHint[MaglorConfig] =
    ProductHint[MaglorConfig](allowUnknownKeys = false)

  given ConfigReader[CaseConverter] =
    ConfigReader.fromNonEmptyStringTry(StringCase(_).map(CaseConverter.apply).toTry)

  given ConfigReader[SQLEngine] =
    ConfigReader.fromNonEmptyStringTry(SQLEngine(_).toTry)

  given ConfigReader[MaglorConfig] =
    ConfigReader.forProduct7[MaglorConfig, Option[SQLEngine],
                             Option[CaseConverter], Option[CaseConverter], Option[CaseConverter],
                             Option[Map[String, String]], Option[Map[String, Map[String, String]]],
                             Option[Map[String, String]]] (
      "engine", "tableCaseConverter", "columnCaseConverter", "operatorCaseConverter",
                "tableConverter", "columnConverter", "operatorConverter") {
      case (engine, tableCaseConverter, columnCaseConverter, operatorCaseConverter,
                    tableConverter, columnConverter, operatorConverter) =>
        MaglorConfig(engine, tableCaseConverter, columnCaseConverter, operatorCaseConverter,
                    tableConverter, columnConverter, operatorConverter)
    }
