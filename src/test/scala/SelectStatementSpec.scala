package test

import org.scalatest.funspec.AnyFunSpec

import com.albertprz.logograph._

class SelectStatementSpec extends AnyFunSpec {

  describe("A Select Statement") {

    it("can serialize simple queries") {

      val simpleQuery = selectAll[Person]

      val simpleQuerySql = """SELECT      p.*
           FROM        [person] AS p"""

      assert(simpleQuery.sql.trimLines() == simpleQuerySql.trimLines())
    }

    it("can serialize complex queries") {

      val complexQuery =
        from[(Person, Address, Telephone)].select { case (p, a, t) =>
          Query(
            Select(Result(p.name, p.age, a.street, 72162183)),
            Where(a.street.like("%Baker St%"), coalesce(p.isEmployer, false)),
            OrderBy(desc(p.age)),
            InnerJoin(t)(t.id === p.telephoneId),
            LeftJoin(a)(a.id === p.addressId)
          )
        }

      val complexQuerySql =
        """SELECT      p.[name] AS [name], p.[age] AS [age], a.[street] AS [street], 72162183 AS [phone_number]
           FROM        [person] AS p
           INNER JOIN  [phone] AS t ON t.[id] = p.[phone_id]
           LEFT JOIN   [address] AS a ON a.[id] = p.[address_id]
           WHERE       (a.[street] LIKE '%Baker St%') AND
                       (COALESCE (p.[is_employer], 0))
           ORDER BY    p.[age] DESC"""

      assert(complexQuery.sql.trimLines() == complexQuerySql.trimLines())
    }

    it("can serialize queries including literal values") {

      val literalValsQuery =
        from[Address].select { a =>
          Query(
            SelectAll(a),
            Where(a.street.in(List("Carnaby St", "Downing St")))
          )
        }

      val literalValsQuerySql = """SELECT      a.*
           FROM        [address] AS a
           WHERE       a.[street] IN ('Carnaby St', 'Downing St')"""

      assert(
        literalValsQuery.sql.trimLines() == literalValsQuerySql.trimLines()
      )
    }

    it("can serialize queries including runtime values") {

      val allowedPhoneNumbers = List(658976534L, 870127465L)

      val runtimeValsQuery =
        from[Telephone].select { t =>
          Query(
            SelectAll(t),
            Where(t.number.in(allowedPhoneNumbers))
          )
        }

      val runtimeValsQuerySql = """SELECT      t.*
           FROM        [phone] AS t
           WHERE       t.[number] IN (?, ?)"""

      assert(
        runtimeValsQuery.sql.trimLines() == runtimeValsQuerySql.trimLines()
      )

      assert(runtimeValsQuery.paramList == allowedPhoneNumbers)
    }

    it("can serialize queries using set operations") {

      val personsQuery1 =
        from[Person].select { p =>
          Query(SelectDistinctAll(p), Where(p.age < 38))
        }

      val personsQuery2 =
        from[Person].select { p =>
          Query(SelectDistinctAll(p), Where(p.name <> "George"))
        }

      val unionQuerySql = """SELECT      DISTINCT p.*
           FROM        [person] AS p
           WHERE       p.[age] < 38

           UNION

           SELECT      DISTINCT p.*
           FROM        [person] AS p
           WHERE       p.[name] <> 'George'"""

      val intersectQuerySql = """SELECT      DISTINCT p.*
          FROM        [person] AS p
          WHERE       p.[age] < 38

          INTERSECT

          SELECT      DISTINCT p.*
          FROM        [person] AS p
          WHERE       p.[name] <> 'George'"""

      assert(
        personsQuery1.union(personsQuery2).sql.trimLines() == unionQuerySql
          .trimLines()
      )

      assert(
        personsQuery1
          .intersect(personsQuery2)
          .sql
          .trimLines() == intersectQuerySql.trimLines()
      )
    }

    it("can serialize deeply nested queries into nested CTEs") {

      val personsQuery =
        from[Person].select { p =>
          Query(
            SelectAll(p),
            Where((p.name === "Mark").or(p.name === "John"), p.age > 25)
          )
        }

      val adressesQuery =
        from[Address].select { a =>
          Query(SelectAll(a), Where(a.street.like("%Baker St%")))
        }

      val telephoneQuery =
        from[Telephone].select { t =>
          Query(SelectAll(t), Where(t.number <> 676874981))
        }

      val innerNestedQuery1 =
        from(personsQuery, adressesQuery).select { case (p, a) =>
          Query(
            Select(InnerResult1(p.name, max(p.age), a.street))
          )
        }

      val innerNestedQuery2 =
        from(personsQuery, telephoneQuery).select { case (p, t) =>
          Query(
            Select(InnerResult2(p.name, t.number))
          )
        }

      val deeplyNestedQuery =
        from(innerNestedQuery1, innerNestedQuery2).select { case (a, b) =>
          Query(
            Select(Result(a.name, a.age, a.street, b.telephoneNumber)),
            OrderBy(asc(a.name)),
            InnerJoin(b)(b.name === a.name)
          )
        }

      val deeplyNestedQuerySql =
        """WITH q1 AS
          (
            SELECT      p.*
            FROM        [person] AS p
            WHERE       (p.[name] = 'Mark' OR p.[name] = 'John') AND
                        (p.[age] > 25)
          ),
          q2 AS
          (
            SELECT      a.*
            FROM        [address] AS a
            WHERE       a.[street] LIKE '%Baker St%'
          ),
          q3 AS
          (
            SELECT      p.[name] AS [name], MAX (p.[age]) AS [age], a.[street] AS [street]
            FROM        [q1] AS p, [q2] AS a
            GROUP BY    p.[name], a.[street]
          ),
          q4 AS
          (
            SELECT      p.*
            FROM        [person] AS p
            WHERE       (p.[name] = 'Mark' OR p.[name] = 'John') AND
                        (p.[age] > 25)
          ),
          q5 AS
          (
            SELECT      t.*
            FROM        [phone] AS t
            WHERE       t.[number] <> 676874981
          ),
          q6 AS
          (
            SELECT      p.[name] AS [name], t.[number] AS [phone_number]
            FROM        [q4] AS p, [q5] AS t
          )

          SELECT      a.[name] AS [name], a.[age] AS [age], a.[street] AS [street], b.[phone_number] AS [phone_number]
          FROM        [q3] AS a
          INNER JOIN  [q6] AS b ON b.[name] = a.[name]
          ORDER BY    a.[name] ASC"""

      assert(
        deeplyNestedQuery.sql.trimLines() == deeplyNestedQuerySql.trimLines()
      )
    }
  }
}
