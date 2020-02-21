package com.book.cats

import cats._
import cats.implicits._

final case class Cat(name: String, age: Int, color: String)

object EqMain extends App {
  val eqInt = Eq[Int]

  println(List(1, 2, 3).map(Option(_)).filter(item => item == 1))
  println(eqInt.eqv(123, 123))
  println(123 === 123)
  println(eqInt.eqv(123, 234))
  println(123.some === none[Int])
  //  println(List(1, 2, 3).map(Option(_)).filter(item => item === 1)) // error
  //  println(eqInt.eqv(123, "234"))//error

  val cat1 = Cat2("Garfield", 38, "orange and black")
  val cat2 = Cat2("Heathcliff", 33, "orange and black")
  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat2]

//  implicit val catEq: Eq[Cat] = (x: Cat, y: Cat) => x == y
  implicit val catEq: Eq[Cat2] = Eq.instance[Cat2] { (c1, c2) =>
    c1.name === c2.name && c1.age === c2.age && c1.color === c2.color
  }

    println(s"cats ${optionCat1 === optionCat2}")
}
