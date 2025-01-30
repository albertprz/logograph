package it

import java.sql.DriverManager
import com.albertprz.logograph.*
import cats.effect.IO

object Database:

  implicit val context: LogographContext[IO] = Connection.context

  def setup(): IO[Unit] =
    context.run(
      insert(SeedData.people),
      insert(SeedData.addresses),
      insert(SeedData.telephones)
    )

  def cleanup(): IO[Unit] =
    context.run(deleteAll[Address], deleteAll[Telephone], deleteAll[Person])

private object Connection:

  lazy val context = new LogographContext[IO](connection)

  private val connection =

    Class.forName("org.sqlite.JDBC");
    DriverManager.getConnection("jdbc:sqlite::memory:")

  private val init =

    val initStatementsSql =
      Seq(
        """CREATE TABLE person (
                                    name text,
                                    age int,
                                    is_employer int,
                                    address_id int,
                                    phone_id int)""",
        "CREATE TABLE address (id int, street text)",
        "CREATE TABLE phone (id int, number string)"
      )

    val stmt = connection.createStatement()

    for stmtSql <- initStatementsSql do stmt.addBatch(stmtSql)

    stmt.executeBatch()
