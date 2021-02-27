package orm

import ast.QueryImpl
import scala.language.experimental.macros

import java.sql.{Connection, DriverManager, ResultSet, Statement}
import scala.util.Try
import scala.collection.mutable.ListBuffer

object QueryBuilder {

  def query[T]() = QueryBuilder[T]()

  def query[T](fromQueries: FullQuery[_, _]*) =
    QueryBuilder[T](subqueries = fromQueries)

  def query[T](fromQuery: FullQuery[_, T]) =
    QueryBuilder[T](subqueries = Seq(fromQuery))

  def query[T, R](fromQuery1: FullQuery[_, T], fromQuery2: FullQuery[_, R]) =
    QueryBuilder[(T, R)](subqueries = Seq(fromQuery1, fromQuery2))

  def query[T, R, X](fromQuery1: FullQuery[_, T], fromQuery2: FullQuery[_, R],
                     fromQuery3: FullQuery[_, X]) =
    QueryBuilder[(T, R, X)](subqueries = Seq(fromQuery1, fromQuery2, fromQuery3))

  def query[T, R, X, S](fromQuery1: FullQuery[_, T], fromQuery2: FullQuery[_, R],
                        fromQuery3: FullQuery[_, X], fromQuery4: FullQuery[_, S]) =
    QueryBuilder[(T, R, X, S)](subqueries = Seq(fromQuery1, fromQuery2, fromQuery3, fromQuery4))

  def query[T, R, X, S, Q](fromQuery1: FullQuery[_, T], fromQuery2: FullQuery[_, R],
                           fromQuery3: FullQuery[_, X], fromQuery4: FullQuery[_, S],
                           fromQuery5: FullQuery[_, Q]) =
    QueryBuilder[(T, R, X, S, Q)](subqueries = Seq(fromQuery1, fromQuery2, fromQuery3,
                                                   fromQuery4, fromQuery5))

  def query[T, R, X, S, Q, H](fromQuery1: FullQuery[_, T], fromQuery2: FullQuery[_, R],
                              fromQuery3: FullQuery[_, X], fromQuery4: FullQuery[_, S],
                              fromQuery5: FullQuery[_, Q], fromQuery6: FullQuery[_, H]) =
    QueryBuilder[(T, R, X, S, Q, H)](subqueries = Seq(fromQuery1, fromQuery2, fromQuery3,
                                                  fromQuery4, fromQuery5, fromQuery6))
}

case class QueryBuilder[T](private val subqueries: Seq[FullQuery[_, _]] = Seq.empty) {

  def select[A <: Product](queryFnTree: T ⇒ Query[A]): FullQuery[T, A] =
    macro QueryImpl.select[T, A]

  def selectDebug[A <: Product](queryFnTree: T ⇒ Query[A]): FullQuery[T, A] =
    macro QueryImpl.selectDebug[T, A]

}

trait Table extends Product {

  val * = this
}


object QueryOps {

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

object ReflectionUtils {

  import scala.reflect.runtime.universe._
  private lazy val universeMirror = runtimeMirror(getClass.getClassLoader)

  def companionOf[T: TypeTag] = {
    val companionMirror = universeMirror.reflectModule(typeOf[T].typeSymbol.companionSymbol.asModule)
    companionMirror.instance.asInstanceOf[Companion]
  }
}

trait Companion {
  def apply(x: Any*): Any

  val paramNames =
    applyMethod.getParameters
                  .map(_.getName)

  val paramTypes =
    applyMethod.getParameterTypes
                  .map(_.getName)

  val paramCount =
    applyMethod.getParameterCount

  private val applyMethod =
    this.getClass.getDeclaredMethods
                 .filter(_.getName == "apply")
                 .find(_.getReturnType.getName != "java.lang.Object").get
}

class ScalaQLContext (conn: Connection) {

  import scala.reflect.runtime.{universe => ru}

  def run [T: ru.TypeTag] (query: FullQuery[_, T]) =
    runQuery(query).get

  private def runQuery [T: ru.TypeTag] (query: FullQuery[_, T]) = Try {

    val companion = ReflectionUtils.companionOf[T]
    val stmt = conn.createStatement
    val resultSet = stmt.executeQuery(query.sql)

    val results = ListBuffer[T]()

    while (resultSet.next()) {
      val ctorArgs = getCtorArgs(resultSet, companion.paramCount, companion.paramTypes)
      results += companion.apply(ctorArgs).asInstanceOf[T]
    }

    results.toList
  }

  private def getCtorArgs(results: ResultSet, paramCount: Int, paramTypes: Array[String]) =
    for (i <- 0 to paramCount)
        yield paramTypes(i) match {
          case "int" => results.getInt(i + 1)
          case "java.lang.String" => results.getString(i + 1)
          case _ => null
        }
}


object Application extends App {

  case class Person(name: String, age: Int, isEmployer: Boolean, addressId: Int, telephoneId: Int)
  case class Address(id: Int, street: String)
  case class Telephone(id: Int, number: String)

  import orm.QueryBuilder._
  import orm.QueryOps._

  val john = Person("", 12, false, 1, 1)
  val names = List("John", "Richard", "Thomas")

  val conn = DriverManager.getConnection("jbdc:sqlite::memory")
  val context = new ScalaQLContext(conn)

  val qry = query[(Person, Address, Telephone)].select {
    case (p, h, t) ⇒ Query(
      Select       (p.name, p.age, h.street, t.number),
      Where        (h.street like "%Baker St%",
                    p.name in names,
                    p.isEmployer,
                    p.age <> john.age),
      OrderBy      (desc (p.age))) (
      LeftJoin (h) (h.id === p.addressId),
      LeftJoin (t) (t.id === p.telephoneId)
    ) }

  println()
  println(qry.sql)
  println(qry.params)
  println()

  val result = context.run(qry)
}
