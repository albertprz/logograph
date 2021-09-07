package test

import com.albertoperez1994.scalaql._
import com.albertoperez1994.scalaql.utils.StringUtils._
import org.scalatest.funspec.AnyFunSpec
import TestModels._

class InsertStatementSpec extends AnyFunSpec {

  describe("An Insert Statement") {


    it("can serialize insert statements from single values") {

      val joe = Person("Joe", 35, true, 1, 4)

      val singleValueInsert = insert(joe)

      val singleValueInsertSql =
        """INSERT INTO [Person] ([name], [age], [isEmployer], [addressId], [telephoneId])
           VALUES      (?, ?, ?, ?, ?)"""


      assert(singleValueInsert.sql.normalized() == singleValueInsertSql.normalized())

      assert(singleValueInsert.paramList == joe.productIterator.toList)
    }


    it("can serialize insert statements from a sequence of values") {

      val joe = Person("Joe", 35, true, 1, 4)
      val mark = Person("Mark", 31, false, 2, 5)

      val sequencedValuesInsert = insert(Seq(joe, mark))

      val sequencedValuesInsertSql =
        """INSERT INTO [Person] ([name], [age], [isEmployer], [addressId], [telephoneId])
           VALUES      (?, ?, ?, ?, ?),
                       (?, ?, ?, ?, ?)"""


      assert(sequencedValuesInsert.sql.normalized() == sequencedValuesInsertSql.normalized())

      assert(sequencedValuesInsert.paramList == Seq(joe, mark).flatMap(_.productIterator).toList)
    }


    it("can serialize insert statements from a query") {

      val adressesQuery = query[Address].select {
        a => Query(
          SelectAll (a),
          Where (a.street like "%Baker St%"))
      }

      val queryInsert = insert(adressesQuery)

      val queryInsertSql =
        """INSERT INTO [Address] ([id], [street])
           SELECT      a.*
           FROM        [Address] AS a
           WHERE       a.[street] like '%Baker St%'"""


      assert(queryInsert.sql.normalized() == queryInsertSql.normalized())
    }
  }
}
