# Logograph

## Summary
This library provides a typesafe Scala DSL for generating SQL queries and statements<br>
These statements can then be executed against a Database using the ***Logograph Context***, <br>
which uses JDBC for connecting to the Database, or any other Database connection middleware<br> provided in the client application<br>


By using Scala macros in order to generate the SQL statetements at compile-time,<br>
SQL generation provides a useful abstraction while having no performance <br>
overhead at runtime compared to running a raw SQL statement.

These SQL generation macros also perform validation on the SQL statements,<br>
so the library user can detect and fix several kinds of errors without the need to execute <br>
the statement against the Database. This also means that most SQL syntax errors are guaranteed<br>
to never happen in production code.

The generated SQL statements can embed runtime values and are fully parameterised,<br>
so there is no risk of SQL Injection attacks.

The DSL works in a similar fashion to some other SQL compile-time DSLs available for Scala,<br>
for instance [Quill](https://github.com/getquill/quill),
but it aims to provide a streamlined API, with a focus on simplicity<br>
from the user standpoint, leaving some advanced functionality aside, in order to adapt better<br>
to the most common business use cases, following a convention over configuration approac.<br>

There is some in-project customization around both global naming conventions as well as explicit table and column names via the use of an optional ***logograph.conf*** file in the project. You can see an example [here](https://github.com/albertprz/logograph/blob/main/src/test/resources/logograph.conf)

For some examples of statements serialisation and execution (such as complex nested queries or query unions & intersections), you can check the [unit](https://github.com/albertprz/logograph/tree/main/src/test/scala) or [integration](https://github.com/albertprz/logograph/tree/main/src/it/scala) test folders. 

## Usage

In order to use this library, you would need to add it to the dependencies in build.sbt:<br>
All the releases are cross-compiled and therefore available for both ***scala 3*** & ***scala 2.13***. <br> All the tests also run for both Scala versions, since the macro implementations differ between the language versions.

```scala
libraryDependencies += "io.github.albertprz" % "logograph" %% "0.1.0")
```

Logograph DSL aim is to reflect as closely as possible the underlying SQL representation,<br>
so the API is very SQL like:

```scala
import com.albertprz.logograph._

val qry = from[(Person, Address, Telephone)].select {
  case (p, a, t) ⇒ Query(
    Select          (Result (p.name, p.age, a.street, t.number))
    Where           (a.street like "%Baker St%",
                      p.name in names,
                      coalesce (p.isEmployer, false)),
    OrderBy         (desc (p.age)),
    LeftJoin (a)    (a.id === p.addressId),
    LeftJoin (t)    (t.id === p.telephoneId))
  }
```

The DSL is very concise and uses the same operator and functions that the SQL equivalent.<br>
The SQL output for this query would be the following:

```sql
SELECT      p.[name], p.[age], a.[street], t.[number]
FROM        [Person] AS p
LEFT JOIN   [Address] AS a ON a.[id] = p.[addressId]
LEFT JOIN   [Telephone] AS t ON t.[id] = p.[telephoneId]
WHERE       (a.[street] like '%Baker St%') AND
            (p.[name] in (?, ?, ?)) AND
            (coalesce (p.[isEmployer], 0))
ORDER BY    p.[age] desc
```

And the parameters that were used in this query, for the runtime values are:

```scala
{@Application.names -> [John, Mark, Thomas]}
```

The query is generated in a fully typesafe manner. The query input tables must be specified by<br>
case classes that extend the ***DbTable*** trait and the query result type must be a case class<br>
that extends either the ***DbTable*** or the ***DbResult*** traits.<br>
The qry value will be an object of type ***SelectStatement[Result]***, in this case.

```scala
// Database Table Models
case class Person (name: String, age: Int, isEmployer: Boolean, addressId: Int, telephoneId: Int)
                    extends DbTable
case class Address (id: Int, street: String) extends DbTable
case class Telephone (id: Int, number: String) extends DbTable


// Query Result Model
case class Result (name: String, age: Int, street: String, telephoneNumber: String) extends DbResult
```

Additionally the SQL Statement API methods ending in ***Debug***, can be used in order to generate<br>
a compile time error that will expose the SQL statement at compile time as well as the internal AST,<br>
that was used to generate the SQL:

```scala
QueryClause (
  SelectClause ([Field (p, name), Field (p, age), Field (a, street), Field (t, number)]),
  FromClause ({p -> Person}), [
  LeftJoinClause (Address, a, [
    Operation (===, [Field (a, id), Field (p, addressId)])]),
  LeftJoinClause (Telephone, t, [
    Operation (===, [Field (t, id), Field (p, telephoneId)])])],
  WhereClause ([
    Operation (like, [Field (a, street),
      LiteralVal ("%Baker St%")]),
    Operation (in, [Field (p, name), Identity (?)]),
    Operation (coalesce, [Field (p, isEmployer), LiteralVal (0)])]),
  OrderByClause ([
    Operation (desc, [Field (p, age)])]))
```

The API also exposes ***Insert***, ***Update*** & ***Delete*** statements, which have a common trait
(***SQLStatefulStament***):

```scala
val stmts = Seq(insert(john),

                insert(johnAddress),

                update[Person] (p => (Map(p.name -> "Mark",
                                          p.age  -> 50),
                                      Where(p.age >= 10))),

                delete[Address] (a => Where(a.street <> "Baker Street"))
```

These statements will generate the following SQL output:

```sql
INSERT INTO [Person] ([name], [age], [isEmployer], [addressId], [telephoneId])
VALUES      (?, ?, ?, ?, ?)

INSERT INTO [Address] ([id], [street])
VALUES      (?, ?)

UPDATE      [Person]
SET         [name] = 'Mark',
            [age] = 50
WHERE       [age] >= 10

DELETE FROM [Address]
WHERE       [street] <> 'Baker Street'
```

The raw SQL and runtime params from any statement can be obtained at runtime <br>
by accessing the ***sql*** and ***params*** fields:

```scala
(qry.sql, qry.params)
```

At last, the statements can be run against a Database by using implicitly / explicitly <br> a in-scope ***LogographContext*** instance, using the appropiate JDBC connection object<br>

```scala
val conn: Connection
val qry: SelectStatement[Person]
val stmt: UpdateStatement[Address]

implicit val context = new LogographContext[IO](conn)
val resultsIO: IO[Seq[Person]] = qry.run()
val operationIO: IO[Unit] = stmt.run()
```
or 

```scala
val conn: Connection
val qry: SelectStatement[Person]
val stmts: List[SQLStatefulStatements]

val context = new LogographContext[IO](conn)
val resultsIO: IO[Seq[Person]] = context.run(qry)
val operationsIO: IO[Unit] = context.run(stmts:_*)
```
