package com.book.cats

import cats.data.{EitherT, Validated}
import cats.syntax.either._
import cats.instances.list._ // for Semigroupal
import cats.syntax.apply._ // for mapN

object ValidatedMain extends App {

  case class User(name: String, age: Int)

  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  def getValue(map: Map[String, String], name: String): FailFast[String] = {
    Either.fromOption(map.get(name), List(s"$name field not specified"))
  }

  def parseInt(fieldName: String)(value: String): FailFast[Int] = {
    Either.catchOnly[NumberFormatException](value.toInt).leftMap(_ => List(s"$fieldName must be an integer"))
  }

  def nonBlank(name: String)(data: String): FailFast[String] =
    Either.cond(data.nonEmpty, data, List(s"$name cannot be blank"))

  def nonNegative(name: String)(data: Int): FailFast[Int] =
    Right(data).
      ensure(List(s"$name must be non-negative"))(_ >= 0)

  def readName(data: Map[String, String]): FailFast[String] =
    for {
      name <- getValue(data, "name")
      r <- nonBlank("name")(name)
    } yield r

  def readAge(data: Map[String, String]): FailFast[Int] = {
    for {
      ageString <- getValue(data, "age")
      age <- parseInt("age")(ageString)
      r <- nonNegative("age")(age)
    } yield r
  }

  def readUser(data: Map[String, String]): FailSlow[User] =
    (
      readName(data).toValidated,
      readAge(data).toValidated
      ).mapN(User.apply)

  println(getValue(Map("name" -> "Bob"), "name"))
  println(getValue(Map("name1" -> "Bob"), "name"))
  println(parseInt("age")("12"))
  println(parseInt("age")("1a2"))


  println(nonBlank("name")("Dade Murphy"))
  // res36: FailFast[String] = Right(Dade Murphy)
  println(nonBlank("name")(""))
  // res37: FailFast[String] = Left(List(name cannot be blank))
  println(nonNegative("age")(11))
  // res38: FailFast[Int] = Right(11)
  println(nonNegative("age")(-1))
  // res39: FailFast[Int] = Left(List(age must be non-negative))

  println(readName(Map("name" -> "Dade Murphy")))
  // res41: FailFast[String] = Right(Dade Murphy)
  println(readName(Map("name" -> "")))
  // res42: FailFast[String] = Left(List(name cannot be blank))
  println(readName(Map()))
  // res43: FailFast[String] = Left(List(name field not specified))
  println(readAge(Map("age" -> "11")))
  // res44: FailFast[Int] = Right(11)
  println(readAge(Map("age" -> "-1")))
  // res45: FailFast[Int] = Left(List(age must be non-negative))
  println(readAge(Map()))
  // res46: FailFast[Int] = Left(List(age field not specified))


  println(readUser(Map("name" -> "Dave", "age" -> "37")))
  // res48: FailSlow[User] = Valid(User(Dave,37))
  println(readUser(Map("age" -> "-1")))
  // res49: FailSlow[User] = Invalid(List(name field not specified, age
//  must be non-negative))
}
