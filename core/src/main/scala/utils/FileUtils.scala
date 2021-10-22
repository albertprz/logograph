package com.albertprz.logograph.utils

import java.nio.file.{Files, Paths}
import java.io.FileReader
import java.util.{Optional, Properties}


object FileUtils:

  def getFile(fileName: String) =
    Files.walk(Paths.get("."))
          .filter(_.getFileName().endsWith(fileName))
          .findAny()
          .toOption()
          .map(_.toFile())


  extension[T] (opt: Optional[T])

    def toOption () =
      if opt.isPresent then Some(opt.get())
      else               None
