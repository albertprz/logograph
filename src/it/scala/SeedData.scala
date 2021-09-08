package it

import TestModels._

object SeedData {

  val john = Person ("John", 50, true, 2, 6)
  val johnAddress = Address (2, "Baker Street")
  val johnTelephone = Telephone (6, 7361127)

  val thomas = Person ("Thomas", 36, false, 3, 7)
  val thomasAddress = Address (3, "Carnaby Street")
  val thomasTelephone = Telephone (7, 62163219)
}
