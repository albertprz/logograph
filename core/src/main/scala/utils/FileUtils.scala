package com.albertoperez1994.scalaql.utils

import java.nio.file.{Files, Paths}
import java.io.FileReader
import java.util.{Optional, Properties}


object FileUtils {

  def getFile(fileName: String) =
    Files.walk(Paths.get("."))
          .filter(_.getFileName().endsWith(fileName))
          .findAny()
          .toOption()
          .map(_.toFile())

  implicit class RichOptional[T] (opt: Optional[T]) {
    def toOption [T] () =
      if (opt.isPresent) Some(opt.get())
      else               None
  }
}
