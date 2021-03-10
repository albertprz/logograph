package example

import java.sql.DriverManager
import orm._
import orm.StatementBuilder._
import orm.QueryOps._
import utils.StringUtils._

object Setup {

  val sqliteConnection = {

    Class.forName("org.sqlite.JDBC");
    DriverManager.getConnection("jdbc:sqlite::memory:")
  }

  def initializeDb () = {

    val sqls = Seq( """ CREATE TABLE Person (
                      name text,
                      age int,
                      isEmployer int,
                      addressId int,
                      telephoneId int) """,

                    "CREATE TABLE Address (id int, street text)",
                    "CREATE TABLE Telephone (id int, number string)")

    val stmt = sqliteConnection.createStatement()

    for (sql <- sqls)
      stmt.addBatch(sql)

    stmt.executeBatch()
  }
}

object Application extends App {


  val conn = Setup.sqliteConnection
  Setup.initializeDb()


  // Database Table Models
  case class Person (name: String, age: Int, isEmployer: Boolean, addressId: Int, telephoneId: Int)
                      extends DbTable
  case class Address (id: Int, street: String) extends DbTable
  case class Telephone (id: Int, number: String) extends DbTable


  // Query Result Model
  case class Result (name: String, age: Int, street: String, telephoneNumber: String) extends DbResult


  val names = List("John", "Richard", "Thomas")
  val john = Person ("John", 50, true, 2, 1)
  val johnAddress = Address (2, "Baker Street")

  val context = new ScalaQLContext(conn)


  val updt = update[Person] (p => Map(p.name -> "John",
                                      p.age -> 12))


  println(updt)

  context.run(insert(john),
              insert(johnAddress))

  val qry = query[(Person, Address, Telephone)].select {
    case (p, h, t) ⇒ Query(
      Select          (Result (p.name, p.age, h.street, t.number)),
      Where           (h.street like "%Baker St%",
                       p.name in names,
                       p.isEmployer),
      OrderBy         (desc (p.age)),
      LeftJoin (h)    (h.id === p.addressId),
      LeftJoin (t)    (t.id === p.telephoneId))
  }

  val results = context.run(qry)

  println(s"$qry \nResults: \n\n${pprint(results)} \n")
}
