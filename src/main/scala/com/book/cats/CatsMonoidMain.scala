package com.book.cats

import cats._
import cats.implicits._
import MonoidInstances2._

case class Order(totalCost: Double, quantity: Double)

object CatsMonoidMain extends App {
  val a = Option(22)
  val b = Option(20)
  println(Monoid[Option[Int]].combine(a, b))

  val intResult = 1 |+| 2 |+| Monoid[Int].empty
  println(intResult)
  println(Adder.add(List(1, 2, 3)))
  println(Adder.add2(List(1, 2, 3)))
  println(Adder.add2(List(1.some, none[Int], 2.some, 3.some)))

  println(Order(10, 12) |+| Order(5, 5))
}

object Adder {
  def add(items: List[Int]) = {
    items.fold(Monoid[Int].empty)(_ |+| _)
  }

  def add2[A: Monoid](items: List[A]): A = {
    items.fold(Monoid[A].empty)(_ |+| _)
  }
}

object MonoidInstances2 {
  implicit val orderMonoid: Monoid[Order] = Monoid.instance[Order](Order(0, 0), (o1, o2) => Order(o1.totalCost + o2.totalCost, o1.quantity + o2.quantity))
}