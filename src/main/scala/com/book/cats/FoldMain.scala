package com.book.cats

import cats.{Eval, Foldable}
import cats.instances.all._

object FoldMain extends App {

  def map[A, B](list: List[A])(func: A => B): List[B] =
    list.foldRight(List.empty[B])((i, a) => func(i) :: a )

  def flatMap[A, B](list: List[A])(func: A => List[B]): List[B] =
    list.foldRight(List.empty[B])((i, a) => func(i) ::: a )

  def filter[A](list: List[A])(func: A => Boolean): List[A] =
    list.foldRight(List.empty[A])((i, a) => if (func(i)) i :: a else a )

  def sumWithMonoid[A](list: List[A])
                      (implicit monoid: cats.Monoid[A]): A =
    list.foldRight(monoid.empty)(monoid.combine)


  println(map(List(1, 2, 3))(_ * 2))


  println(flatMap(List(1, 2, 3))(a => List(a, a * 10, a * 100)))
  // res10: List[Int] = List(1, 10, 100, 2, 20, 200, 3, 30, 300)


  println(filter(List(1, 2, 3))(_ % 2 == 1))
  // res11: List[Int] = List(1, 3)

  println(sumWithMonoid(List(1, 2, 3)))
  // res16: Int = 6



  def bigData = (1 to 1000000).toStream
  println(bigData.foldRight(0L)(_ + _))
//   java.lang.StackOverflowError ...



  //stack safe
  val eval: Eval[Long] = Foldable[Stream].
    foldRight(bigData, Eval.now(0L)) { (num, eval) => eval.map(_ + num)
    }

  println(eval.value)


  val ints = List(Vector(1, 2, 3), Vector(4, 5, 6))
  println((Foldable[List] compose Foldable[Vector]).combineAll(ints))
  // res15: Int = 21

}
