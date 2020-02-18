package com.book.cats

import cats.instances.function._ // for Functor
import cats.syntax.functor._

object FunctorsMain {
  def main(args: Array[String]): Unit = {
    val func1: Int => Double =
      (x: Int) => x.toDouble

    val func2: Double => Double =
      (y: Double) => y * 2

    val funcResult1 = (func1 map func2) (1)
    println(funcResult1)

    val funcResult2 = (func1 andThen func2) (1)
    println(funcResult2)

    val funcResult3 = func2(func1(1))
    println(funcResult3)

    val func =
      ((x: Int) => x.toDouble).
        map(x => x + 1).
        map(x => x * 2).
        map(x => x + "!")

    println(func(123))

  }
}
