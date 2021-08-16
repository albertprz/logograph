package com.albertoperez1994.scalaql.config

import com.albertoperez1994.scalaql.utils.{FileUtils, Error, CaseConverter, CamelCase}
import Error.InvalidScalaQLConfig

import pureconfig.generic.auto._
import pureconfig.{ConfigFieldMapping, ConfigReader, ConfigSource}
import pureconfig.generic.ProductHint

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import io.circe.{Decoder, Encoder}
import scala.util.Properties



case class ScalaQLConfig (tableConverter: Option[CaseConverter] = None,
                          columnConverter: Option[CaseConverter] = None,
                          operatorConverter: Option[CaseConverter] = None)


object ScalaQLConfig {

  val configFileName = "scalaql.conf"

  val get = fetchConfig() match {
    case Some(conf) => conf.toTry.get
    case None       => memoizeConfig()
  }

  private implicit val configReader: ConfigReader[ScalaQLConfig] = ConfigReader[ScalaQLConfig]

  private implicit val productHint = ProductHint[ScalaQLConfig](new ConfigFieldMapping {
    def apply(fieldName: String) = CaseConverter(CamelCase).convertCase(fieldName)
  })


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
