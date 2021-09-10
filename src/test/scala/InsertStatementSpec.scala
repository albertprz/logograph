package test

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import com.albertoperez1994.scalaql._
import com.albertoperez1994.scalaql.utils.StringUtils._
import TestModels._

class InsertStatementSpec extends AnyFunSpec with Matchers {

  describe("An Insert Statement") {


    it("can serialize insert statements from single values") {

      val joe = Person("Joe", 35, true, 1, 4)

      val singleValueInsert = insert(joe)

      val singleValueInsertSql =
        """INSERT INTO [person] ([name], [age], [is_employer], [address_id], [telephone_id])
           VALUES      (?, ?, ?, ?, ?)"""


      singleValueInsert.sql.normalized() should equal (singleValueInsertSql.normalized())

      singleValueInsert.paramList should equal (joe.productIterator.toList)
    }


    it("can serialize insert statements from a sequence of values") {

      val joe = Person("Joe", 35, true, 1, 4)
      val mark = Person("Mark", 31, false, 2, 5)

      val sequencedValuesInsert = insert(Seq(joe, mark))

      val sequencedValuesInsertSql =
        """INSERT INTO [person] ([name], [age], [is_employer], [address_id], [telephone_id])
           VALUES      (?, ?, ?, ?, ?),
                       (?, ?, ?, ?, ?)"""


      sequencedValuesInsert.sql.normalized() should equal (sequencedValuesInsertSql.normalized())

      sequencedValuesInsert.paramList should equal (Seq(joe, mark).flatMap(_.productIterator).toList)
    }


    it("can serialize insert statements from a query") {

      val adressesQuery = query[Address].select {
        a => Query(
          SelectAll (a),
          Where (a.street like "%Baker St%"))
      }

      val queryInsert = insert(adressesQuery)

      val queryInsertSql =
        """INSERT INTO [address] ([id], [street])
           SELECT      a.*
           FROM        [address] AS a
           WHERE       a.[street] LIKE '%Baker St%'"""


      queryInsert.sql.normalized() should equal (queryInsertSql.normalized())
    }
  }
}
