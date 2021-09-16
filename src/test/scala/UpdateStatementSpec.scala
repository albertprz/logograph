package test

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import com.albertoperez1994.scalaql._

class UpdateStatementSpec extends AnyFunSpec with Matchers {

  describe("An Update Statement") {


    it("can serialize update all statements") {

      val simpleUpdate = updateAll[Person] (p =>
        Map(p.age -> (p.age + 5)))

      val simpleUpdateSql =
        """UPDATE      [person]
           SET         [age] = [age] + 5"""


      simpleUpdate.sql.trimLines() should equal (simpleUpdateSql.trimLines())
    }


    it("can serialize filtered update statements") {

      val filteredUpdate = update[Person] (p =>
        (Map  (p.name -> "John"),
         Where(p.name === "Peter")))

      val filteredUpdateSql =
      """UPDATE      [person]
         SET         [name] = 'John'
         WHERE       [name] = 'Peter'"""


      filteredUpdate.sql.trimLines() should equal (filteredUpdateSql.trimLines())
    }
  }
}
