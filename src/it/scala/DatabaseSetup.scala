package it

import java.sql.DriverManager
import com.albertoperez1994.scalaql._
import cats.effect.IO


object Database {

  implicit val context: ScalaQLContext[IO] = Connection.context

  def setup(): IO[Unit] = context.run (insert(SeedData.people),
                                       insert(SeedData.addresses),
                                       insert(SeedData.telephones))


  def cleanup(): IO[Unit] = context.run (deleteAll[Address],
                                         deleteAll[Telephone],
                                         deleteAll[Person])

}

private object Connection {

  lazy val context = new ScalaQLContext[IO](connection)

  private val connection = {

    Class.forName("org.sqlite.JDBC");
    DriverManager.getConnection("jdbc:sqlite::memory:")
  }

  private val init = {

    val initStatementsSql = Seq( """CREATE TABLE Person (
                                    name text,
                                    age int,
                                    isEmployer int,
                                    addressId int,
                                    telephoneId int)""",

                                  "CREATE TABLE Address (id int, street text)",
                                  "CREATE TABLE Telephone (id int, number string)")

    val stmt = connection.createStatement()

    for stmtSql <- initStatementsSql do
      stmt.addBatch(stmtSql)

    stmt.executeBatch()
  }
}
