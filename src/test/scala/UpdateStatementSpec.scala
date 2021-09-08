package test

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import com.albertoperez1994.scalaql._
import com.albertoperez1994.scalaql.utils.StringUtils._
import TestModels._

class UpdateStatementSpec extends AnyFunSpec with Matchers {

  describe("An Update Statement") {


    it("can serialize update all statements") {

      val simpleUpdate = updateAll[Person] (p =>
        Map(p.age -> (p.age + 5)))

      val simpleUpdateSql =
        """UPDATE      [Person]
           SET         [age] = [age] + 5"""


      simpleUpdate.sql.normalized() should equal (simpleUpdateSql.normalized())
    }


    it("can serialize filtered update statements") {

      val filteredUpdate = update[Person] (p =>
        (Map  (p.name -> "John"),
         Where(p.name === "Peter")))

      val filteredUpdateSql =
      """UPDATE      [Person]
         SET         [name] = 'John'
         WHERE       [name] = 'Peter'"""


      filteredUpdate.sql.normalized() should equal (filteredUpdateSql.normalized())
    }
  }
}
