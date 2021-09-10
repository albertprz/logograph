package com.albertoperez1994.scalaql.config

import com.albertoperez1994.scalaql.core.SQLEngine
import com.albertoperez1994.scalaql.utils.{FileUtils, Error, CaseConverter, StringCase}
import StringCase.CamelCase
import Error.InvalidScalaQLConfig

import pureconfig.{ConfigReader, ConfigSource}


case class ScalaQLConfig (engine:                Option[SQLEngine]     = None,
                          tableCaseConverter:    Option[CaseConverter] = None,
                          columnCaseConverter:   Option[CaseConverter] = None,
                          operatorCaseConverter: Option[CaseConverter] = None)


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

  implicit val caseConverterConfigReader: ConfigReader[CaseConverter] =
    ConfigReader.fromNonEmptyStringTry(StringCase(_).map(CaseConverter.apply).toTry)

  implicit val engineConfigReader: ConfigReader[SQLEngine] =
    ConfigReader.fromNonEmptyStringTry(SQLEngine(_).toTry)

  implicit val configReader: ConfigReader[ScalaQLConfig] =
    ConfigReader.forProduct4[ScalaQLConfig, Option[SQLEngine], Option[CaseConverter],
                             Option[CaseConverter], Option[CaseConverter]] (
     "engine","tableCaseConverter", "columnCaseConverter", "operatorCaseConverter") {
      case (engine, table, column, operator) => ScalaQLConfig(engine, table, column, operator)
    }
}
