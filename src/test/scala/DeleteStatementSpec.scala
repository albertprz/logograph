package test

import com.albertoperez1994.scalaql._
import com.albertoperez1994.scalaql.utils.StringUtils._
import org.scalatest.funspec.AnyFunSpec
import TestModels._

class DeleteStatementSpec extends AnyFunSpec {

  describe("A Delete Statement") {


    it("can serialize delete all statements") {

      val simpleDelete = deleteAll[Address]()

      val simpleDeleteSql =
        """DELETE FROM [Address]"""


      assert(simpleDelete.sql.normalized() == simpleDeleteSql.normalized())
    }


    it("can serialize filtered delete statements") {

      val filteredDelete = delete[Telephone] (t =>
        Where(t.number in List(16792021, 72181292)))

      val filteredDeleteSql =
        """DELETE FROM [Telephone]
           WHERE       [number] in (16792021, 72181292)"""


      assert(filteredDelete.sql.normalized() == filteredDeleteSql.normalized())
    }
  }
}
