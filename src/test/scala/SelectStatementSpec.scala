package test

import com.albertoperez1994.scalaql._
import com.albertoperez1994.scalaql.utils.StringUtils._
import org.scalatest.funspec.AnyFunSpec
import TestModels._

class SelectStatementSpec extends AnyFunSpec {

  import ResultModels._

  describe("A SelectStatement") {

    it("can serialize simple queries") {

      val simpleQuery = queryAll[Person]

      val simpleQuerySql = """
        SELECT      p.*
        FROM        [Person] AS p"""

      assert(simpleQuery.sql.normalized() == simpleQuerySql.normalized())
    }

    it("can serialize complex queries") {

      val complexQuery = query[(Person, Address, Telephone)].select {
        case (p, a, t) => Query(
          Select          (Result (p.name, p.age, a.street, 4)),
          Where           (a.street like "%Baker St%",
                            coalesce (p.isEmployer, false)),
          OrderBy         (desc (p.age)),
          LeftJoin (a)    (a.id === p.addressId),
          LeftJoin (t)    (t.id === p.telephoneId))
      }

      val complexQuerySql = """
          SELECT      p.[name], p.[age], a.[street], 4
          FROM        [Person] AS p
          LEFT JOIN   [Address] AS a ON a.[id] = p.[addressId]
          LEFT JOIN   [Telephone] AS t ON t.[id] = p.[telephoneId]
          WHERE       (a.[street] like '%Baker St%') and
                      (coalesce (p.[isEmployer], 0))
          ORDER BY    p.[age] desc"""

      assert(complexQuery.sql.normalized() == complexQuerySql.normalized())
    }

    it("can serialize nested queries into nested CTEs") {

      val personsQuery = query[Person].select {
        p => Query(
          SelectAll (p),
          Where (p.name === "Mark" or p.name === "John",
                 p.age > 25))
      }

      val adressesQuery = query[Address].select {
        a => Query(
          SelectAll (a),
          Where (a.street like "%Baker St%"))
      }

      val streetsQuery = query[Telephone].select {
        t => Query(
          SelectAll (t),
          Where (t.number === 676874981))
      }

      val nestedQuery = query(personsQuery, adressesQuery, streetsQuery).select {
        case (p, a, t) => Query(
          Select          (Result (p.name, p.age, a.street, t.number)),
          OrderBy         (asc (p.name)),
          LeftJoin (a)    (a.id === p.addressId),
          LeftJoin (t)    (t.id === p.telephoneId))
      }


      val nestedQuerySql =
        """WITH q1 AS
           (
             SELECT      p.*
             FROM        [Person] AS p
             WHERE       (p.[name] = 'Mark' or p.[name] = 'John') and
                         (p.[age] > 25)
           ),
           q2 AS
           (
             SELECT      a.*
             FROM        [Address] AS a
             WHERE       a.[street] like '%Baker St%'
            ),
           q3 AS
           (
             SELECT      t.*
             FROM        [Telephone] AS t
             WHERE       t.[number] = 676874981
           )

           SELECT      p.[name], p.[age], a.[street], t.[number]
           FROM        [q1] AS p
           LEFT JOIN   [q2] AS a ON a.[id] = p.[addressId]
           LEFT JOIN   [q3] AS t ON t.[id] = p.[telephoneId]
           ORDER BY    p.[name] asc"""

      assert(nestedQuery.sql.normalized() == nestedQuerySql.normalized())
    }
  }
}

object ResultModels {

  case class Result (name: String, age: Int, street: String, telephoneNumber: Long) extends DbResult
}
