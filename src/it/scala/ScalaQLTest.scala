package it

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.should.Matchers
import cats.effect.unsafe.implicits.global

import com.albertoperez1994.scalaql._
import Database.context


class ScalaQLTest extends AnyFunSpec with BeforeAndAfter with Matchers {

  before {
    Database.setup().unsafeRunSync()
  }

  after {
    Database.cleanup().unsafeRunSync()
  }


  describe("ScalaQL") {


    it("can retrieve query results") {

      val complexQuery = query[(Person, Address, Telephone)].select {
        case (p, a, t) => Query(
          Select          (Result (p.name, p.age, a.street, t.number)),
          Where           (a.street like "%Baker St%",
                            coalesce (p.isEmployer, false)),
          InnerJoin (t)   (t.id === p.telephoneId),
          InnerJoin (a)   (a.id === p.addressId))
      }


      complexQuery.run().unsafeRunSync().head should equal (Result(SeedData.john.name, SeedData.john.age,
                                                                   SeedData.johnAddress.street, SeedData.johnTelephone.number))
    }


    it("can insert records into the database") {

      val matt = Person("Matt", 34, false, 4, 8)
      val mattAddress = Address(4, "Downing St")
      val mattTelephone = Telephone(8, 73216284)

      context.run(insert(matt),
                  insert(mattAddress),
                  insert(mattTelephone))
              .unsafeRunSync()

      queryAll[Person].run().unsafeRunSync() should equal (SeedData.people :+ matt)

      queryAll[Address].run().unsafeRunSync() should equal (SeedData.addresses :+ mattAddress)

      queryAll[Telephone].run().unsafeRunSync() should equal (SeedData.telephones :+ mattTelephone)
    }


    it("can update records from the database") {

      context.run(update[Person](p => (Map(p.age -> 40,
                                          p.isEmployer -> true),
                                      Where(p.name === "John")))).unsafeRunSync()

      val simpleQuery = queryAll[Person]


      simpleQuery.run().unsafeRunSync() should equal (Seq(SeedData.john.copy(age = 40, isEmployer = true),
                                                          SeedData.thomas))
    }


    it("can delete records from the database") {

      context.run(delete[Person](p => Where(p.age < 33)),
                  delete[Address](a => Where(a.street notLike "%Carnaby%")),
                  delete[Telephone](t => Where(t.number <> 62163219)))
              .unsafeRunSync()


      queryAll[Person].run().unsafeRunSync().head should equal (SeedData.thomas)

      queryAll[Address].run().unsafeRunSync().head should equal (SeedData.thomasAddress)

      queryAll[Telephone].run().unsafeRunSync().head should equal (SeedData.thomasTelephone)
    }
  }
}
