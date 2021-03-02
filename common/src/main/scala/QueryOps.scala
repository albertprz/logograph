package orm

object QueryOps {

  val infixOps = List("-", "+", "*", "/", "===", "<>", "&&", "||", "<", ">", "like", "in")

  val postfixOps = List("desc", "asc")

  val prefixOps = List("unary_!", "isNull", "count", "sum", "avg", "max", "min", "stringAgg")

  val allOps = infixOps ++ postfixOps ++ prefixOps

  val aggOps = List("count", "sum", "avg", "max", "min", "stringAgg")

  val opsConversion = Map("unary_!" -> "not", "&&" -> "and", "||" -> "or",
                           "===" -> "=", "isNull" -> "isnull")



  // Aggregate Functions
  def sum[T <: AnyVal] (x: T): T = ???
  def avg[T <: AnyVal] (x: T): T = ???
  def max[T <: AnyVal] (x: T): T = ???
  def min[T <: AnyVal] (x: T): T = ???
  def stringAgg (x: String): String = ???
  def count (x: Any): Int = ???

  // isNull Function
  def isNull (x: Int, y: Int): Int = ???
  def isNull (x: Long, y: Long): Long = ???
  def isNull (x: Float, y: Float): Float = ???
  def isNull (x: Double, y: Double): Double = ???
  def isNull (x: String, y: String): String = ???

  // Order By Functions
  def asc[T <: AnyVal] (x: T): T = ???
  def asc (x: String): String = ???
  def desc[T <: AnyVal] (x: T): T = ???
  def desc (x: String): String = ???

  // Infix Functions for Value Types (Number & Booleans)
  implicit class RichAnyVal[T <: AnyVal] (x: T) {

    def === (y: T): Boolean = ???
    def <>  (y: T): Boolean = ???
    def in  (y: Seq[T]): Boolean = ???
  }

  // Infix Functions for Strings
  implicit class RichString (x: String) {

    def === (y: String): Boolean = ???
    def <>  (y: String): Boolean = ???
    def in (y: Seq[String]): Boolean = ???
    def like (y: String): Boolean = ???
  }
}
