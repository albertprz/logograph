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

  val initializeDb = {

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

  // Database Table Models
  case class Person (name: String, age: Int, isEmployer: Boolean, addressId: Int, telephoneId: Int)
                      extends DbTable
  case class Address (id: Int, street: String) extends DbTable
  case class Telephone (id: Int, number: String) extends DbTable


  // Query Result Model
  case class Result (name: String, age: Int, street: String, telephoneNumber: String) extends DbResult



  val names = List("John", "Mark", "Thomas")
  val john = Person ("John", 50, true, 2, 1)
  val johnAddress = Address (2, "Baker Street")

  implicit val context = new ScalaQLContext(conn)

  val stmts = Seq(insert(john),
                  insert(johnAddress),
                  updateWhere[Person] (p => (Map(p.name -> "Mark",
                                            p.age  -> 50),
                                             Where(p.age >= 10))),
                  deleteWhere[Person] (p => Where(p.name like "M%k")))

  context.run(stmts:_*)

  val qry = query[(Person, Address, Telephone)].select {
    case (p, a, t) ⇒ Query(
      Select          (Result (p.name, p.age, a.street, t.number)),
      Where           (a.street like "%Baker St%",
                       p.name in names,
                       coalesce(p.isEmployer, false)),
      OrderBy         (desc (p.age)),
      LeftJoin (a)    (a.id === p.addressId),
      LeftJoin (t)    (t.id === p.telephoneId))
  }

  val results = qry.run()

  println(stmts.mkString("\n\n", "", ""))
  println(s"$qry \nResults: \n\n${pprint(results)} \n")
}
