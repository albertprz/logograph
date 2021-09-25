package test

import org.scalatest.funspec.AnyFunSpec

import com.albertprz.maglor._


class DeleteStatementSpec extends AnyFunSpec {

  describe("A Delete Statement") {


    it("can serialize delete all statements") {

      val simpleDelete = deleteAll[Address]

      val simpleDeleteSql =
        """DELETE FROM [address]"""


      assert (simpleDelete.sql.trimLines() == simpleDeleteSql.trimLines())
    }


    it("can serialize filtered delete statements") {

      val filteredDelete = delete[Telephone] (t =>
        Where(t.number in List(16792021, 72181292)))

      val filteredDeleteSql =
        """DELETE FROM [phone]
           WHERE       [number] IN (16792021, 72181292)"""


      assert (filteredDelete.sql.trimLines() == filteredDeleteSql.trimLines())
    }
  }
}
