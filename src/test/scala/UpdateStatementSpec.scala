package test

import org.scalatest.funspec.AnyFunSpec

import com.albertprz.logograph._

class UpdateStatementSpec extends AnyFunSpec {

  describe("An Update Statement") {

    it("can serialize update all statements") {

      val simpleUpdate = updateAll[Person](p => Map(p.age -> (p.age + 5)))

      val simpleUpdateSql = """UPDATE      [person]
           SET         [age] = [age] + 5"""

      assert(simpleUpdate.sql.trimLines() == simpleUpdateSql.trimLines())
    }

    it("can serialize filtered update statements") {

      val filteredUpdate =
        update[Person](p => (Map(p.name -> "John"), Where(p.name === "Peter")))

      val filteredUpdateSql = """UPDATE      [person]
         SET         [name] = 'John'
         WHERE       [name] = 'Peter'"""

      assert(filteredUpdate.sql.trimLines() == filteredUpdateSql.trimLines())
    }
  }
}
