package example

import java.sql.DriverManager
import orm._
import orm.QueryBuilder._
import orm.QueryOps._


object Application extends App {


  case class Person(name: String, age: Int, isEmployer: Boolean, addressId: Int, telephoneId: Int)
  case class Address(id: Int, street: String)
  case class Telephone(id: Int, number: String)

  val john = Person("", 50, false, 1, 1)
  val names = List("John", "Richard", "Thomas")


  Class.forName("org.sqlite.JDBC");
  val conn = DriverManager.getConnection("jdbc:sqlite::memory:")

  val sqls = Seq( """ CREATE TABLE Person (
                    name text,
                    age int,
                    isEmployer int,
                    addressId int,
                    telephoneId int) """,

                  "CREATE TABLE Address (id int, street text)",
                  "CREATE TABLE Telephone (id int, number string)",
                  "INSERT INTO Person VALUES ('John', 34, 1, 2, 3)",
                  "INSERT INTO Address VALUES (2, 'Baker Street')")

  val stmt = conn.createStatement()

  for (sql <- sqls)
    stmt.addBatch(sql)

  stmt.executeBatch()

  val context = new ScalaQLContext(conn)

  case class Result (name: String, age: Int, street: String, telephoneNumber: String)

  val qry = query[(Person, Address, Telephone)].select {
    case (p, h, t) ⇒ Query(
      Select       (Result(p.name, p.age, h.street, t.number)),
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

  println(result)
  println()
}
