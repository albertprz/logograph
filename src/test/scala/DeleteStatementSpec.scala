package test

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import com.albertoperez1994.scalaql._
import com.albertoperez1994.scalaql.utils.StringUtils._
import TestModels._

class DeleteStatementSpec extends AnyFunSpec with Matchers {

  describe("A Delete Statement") {


    it("can serialize delete all statements") {

      val simpleDelete = deleteAll[Address]()

      val simpleDeleteSql =
        """DELETE FROM [address]"""


      simpleDelete.sql.normalized() should equal (simpleDeleteSql.normalized())
    }


    it("can serialize filtered delete statements") {

      val filteredDelete = delete[Telephone] (t =>
        Where(t.number in List(16792021, 72181292)))

      val filteredDeleteSql =
        """DELETE FROM [telephone]
           WHERE       [number] IN (16792021, 72181292)"""


      filteredDelete.sql.normalized() should equal (filteredDeleteSql.normalized())
    }
  }
}
