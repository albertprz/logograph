package object test {

  implicit class TestRichString(str: String) {

    def mapLines(mapFn: String => String) =
      str
        .split("\n")
        .map(mapFn)
        .mkString("\n")

    def trimLines() =
      str
        .mapLines(_.trim())
        .trim()
  }
}
