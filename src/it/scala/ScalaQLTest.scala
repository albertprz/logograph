package it

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.should.Matchers
import cats.effect.unsafe.implicits.global

import com.albertoperez1994.scalaql._
import TestModels._
import TestResultModels._
import Database.context


class ScalaQLTest extends AnyFunSpec with BeforeAndAfter with Matchers {

  before {
    Database.setup().unsafeRunSync()
  }

  after {
    Database.cleanup().unsafeRunSync()
  }


  describe("ScalaQL") {


    it("can retrieve complex query results") {

      val complexQuery = query[(Person, Address, Telephone)].select {
        case (p, a, t) => Query(
          Select          (Result (p.name, p.age, a.street, t.number)),
          Where           (a.street like "%Baker St%",
                            coalesce (p.isEmployer, false)),
          InnerJoin (t)   (t.id === p.telephoneId),
          InnerJoin (a)   (a.id === p.addressId))
      }

      val results = List(Result("John", 50, "Baker Street", 7361127))

      complexQuery.run().unsafeRunSync() should equal (results)
    }
  }
}
