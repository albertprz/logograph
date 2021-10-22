package it

import com.albertprz.logograph.DbTable


case class Person (name: String, age: Int, isEmployer: Boolean, addressId: Int, telephoneId: Int)
                    extends DbTable
case class Address (id: Int, street: String) extends DbTable
case class Telephone (id: Int, number: Long) extends DbTable
