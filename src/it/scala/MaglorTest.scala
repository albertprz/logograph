package it

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.BeforeAndAfter
import cats.effect.unsafe.implicits.global

import com.albertprz.maglor._
import Database.context


class MaglorTest extends AnyFunSpec with BeforeAndAfter {

  before {
    Database.setup().unsafeRunSync()
  }

  after {
    Database.cleanup().unsafeRunSync()
  }


  describe("Maglor") {


    it("can retrieve query results") {

      val complexQuery = from[(Person, Address, Telephone)].select { case (p, a, t) =>
        Query(
          Select          (Result (p.name, p.age, a.street, t.number)),
          Where           (a.street like "%Baker St%",
                            coalesce (p.isEmployer, false)),
          InnerJoin (t)   (t.id === p.telephoneId),
          InnerJoin (a)   (a.id === p.addressId))
      }


      assert (complexQuery.run().unsafeRunSync().head == (Result(SeedData.john.name, SeedData.john.age,
                                                                 SeedData.johnAddress.street, SeedData.johnTelephone.number)))
    }


    it("can insert records into the database") {

      val matt = Person("Matt", 34, false, 4, 8)
      val mattAddress = Address(4, "Downing St")
      val mattTelephone = Telephone(8, 73216284)

      context.run(insert(matt),
                  insert(mattAddress),
                  insert(mattTelephone))
              .unsafeRunSync()


      assert (selectAll[Person].run().unsafeRunSync() == SeedData.people :+ matt)

      assert (selectAll[Address].run().unsafeRunSync() == SeedData.addresses :+ mattAddress)

      assert (selectAll[Telephone].run().unsafeRunSync() == SeedData.telephones :+ mattTelephone)
    }


    it("can update records from the database") {

      context.run(update[Person](p => (Map(p.age -> 40,
                                           p.isEmployer -> true),
                                       Where(p.name === "John")))).unsafeRunSync()


      assert (selectAll[Person].run().unsafeRunSync() == Seq(SeedData.john.copy(age = 40, isEmployer = true),
                                                             SeedData.thomas))
    }


    it("can delete records from the database") {

      context.run(delete[Person]   (p => Where(p.age < 33)),
                  delete[Address]  (a => Where(a.street notLike "%Carnaby%")),
                  delete[Telephone](t => Where(t.number <> 62163219)))
              .unsafeRunSync()


      assert (selectAll[Person].run().unsafeRunSync().head == SeedData.thomas)

      assert (selectAll[Address].run().unsafeRunSync().head == SeedData.thomasAddress)

      assert (selectAll[Telephone].run().unsafeRunSync().head == SeedData.thomasTelephone)
    }
  }
}
