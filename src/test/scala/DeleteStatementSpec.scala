package test

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.{should, equal}

import com.albertoperez1994.scalaql._

class DeleteStatementSpec extends AnyFunSpec {

  describe("A Delete Statement") {


    it("can serialize delete all statements") {

      val simpleDelete = deleteAll[Address]

      val simpleDeleteSql =
        """DELETE FROM [address]"""


      simpleDelete.sql.trimLines() should equal (simpleDeleteSql.trimLines())
    }


    it("can serialize filtered delete statements") {

      val filteredDelete = delete[Telephone] (t =>
        Where(t.number in List(16792021, 72181292)))

      val filteredDeleteSql =
        """DELETE FROM [phone]
           WHERE       [number] IN (16792021, 72181292)"""


      filteredDelete.sql.trimLines() should equal (filteredDeleteSql.trimLines())
    }
  }
}
