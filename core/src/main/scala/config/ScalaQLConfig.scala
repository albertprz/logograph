package com.albertoperez1994.scalaql.config

import com.albertoperez1994.scalaql.utils.{FileUtils, Error, CaseConverter, StringCase, CamelCase}
import Error.InvalidScalaQLConfig

import pureconfig.generic.auto._
import pureconfig.{ConfigFieldMapping, ConfigReader, ConfigSource}
import pureconfig.generic.ProductHint

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import io.circe.{Decoder, Encoder}
import scala.util.Properties


case class ScalaQLConfig (tableCaseConverter:    Option[CaseConverter] = None,
                          columnCaseConverter:   Option[CaseConverter] = None,
                          operatorCaseConverter: Option[CaseConverter] = None)

object ScalaQLConfig {

  val configFileName = "scalaql.conf"

  val get = fetchConfig() match {
    case Some(conf) => conf.toTry.get
    case None       => memoizeConfig()
  }

  import Implicits._

  private def fetchConfig() = Properties.envOrNone(configFileName)
                                        .map(decode[ScalaQLConfig])

  private def memoizeConfig() = {
    val conf = FileUtils.getFile(configFileName)
                        .map(file => ConfigSource.file(file)
                                                  .load[ScalaQLConfig]
                                                  .getOrElse(throw new InvalidScalaQLConfig))
                        .getOrElse(ScalaQLConfig())

    Properties.setProp(configFileName, conf.asJson.noSpaces)
    conf
  }
}

object Implicits {

  private implicit val caseConverterConfigReader: ConfigReader[CaseConverter] =
    ConfigReader.fromNonEmptyStringTry(caseName => StringCase(caseName).map(CaseConverter.apply).toTry)

  private implicit val configReader: ConfigReader[ScalaQLConfig] = ConfigReader[ScalaQLConfig]

  private implicit val encoder: Encoder[CaseConverter] = new Encoder[CaseConverter] {
    def apply(caseConverter: CaseConverter): Json =
      Json.fromString(caseConverter.to.caseName)
  }

  private implicit val decoder: Decoder[CaseConverter] = new Decoder[CaseConverter] {
    def apply(c: HCursor): Decoder.Result[CaseConverter] =
      for (toCase <- c.as[String])
        yield CaseConverter(StringCase(toCase).toTry.get)
  }


  private implicit val productHint = ProductHint[ScalaQLConfig](new ConfigFieldMapping {
    def apply(fieldName: String) = CaseConverter(CamelCase).convertCase(fieldName)
  })
}
