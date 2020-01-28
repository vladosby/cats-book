package com.book.cats

import cats._
import cats.implicits._
import java.util.Date
import ShowInstances._

final case class Cat(name: String, age: Int, color: String)

object ShowCatsMain extends App {
  val showInt = Show.apply[Int]

  println(showInt.show(123))
  println(234.show)
  println(new Date().show)
  println(Cat(name = "Cat name", age = 12, color = "blue").show)
}

object ShowInstances {
  //  implicit val dateShow: Show[Date] = (t: Date) => s"${t.getTime} ms since the epoch."
  implicit val dateShow: Show[Date] = Show.show(date => s"${date.getTime} ms since the epoch.")
  implicit val catShow: Show[Cat] = Show.show(cat => s"${cat.name} is a ${cat.age} year-old ${cat.color} cat.")

}
