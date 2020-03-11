package com.book.cats
import cats.instances.future._
import cats.instances.int._
import cats.instances.string._
import cats.instances.vector._
import cats.syntax.foldable._
import cats.syntax.semigroup._
import cats.syntax.traverse._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.postfixOps


object MapReduceMain extends App {


  /** Single-threaded map-reduce function.
    * Maps `func` over `values` and reduces using a `Monoid[B]`. */
  def foldMap[A, B: cats.Monoid](values: Vector[A])(func: A => B): B = {
    values.foldLeft(cats.Monoid[B].empty)(_ |+| func(_))
  }

  def parallelFoldMap[A, B : cats.Monoid](values: Vector[A])(func: A => B): Future[B] = {
//
//       Calculate the number of items to pass to each CPU:
    val numCores = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt

//       Create one group for each CPU:
    val groups: Iterator[Vector[A]] = values.grouped(groupSize)

//    val futures = groups.map(g => Future(g.foldLeft(cats.Monoid[B].empty)(_ |+| func(_))))
    val futures = groups.map(g => Future(foldMap(g)(func)))

    Future.sequence(futures).map(_.foldLeft(cats.Monoid[B].empty)(_ |+| _))
  }

  def parallelFoldMap2[A, B: cats.Monoid]
  (values: Vector[A])
  (func: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt

    values
      .grouped(groupSize)
      .toVector
      .traverse(group => Future(group.toVector.foldMap(func))).map(_.combineAll)
  }

  println(foldMap(Vector(1, 2, 3))(identity))
  // res2: Int = 6

  // Mapping to a String uses the concatenation monoid:
  println(foldMap(Vector(1, 2, 3))(_.toString + "! "))
  // res4: String = "1! 2! 3! "

  // Mapping over a String to produce a String:
  println(foldMap("Hello world!".toVector)(_.toString.toUpperCase))
  // res6: String = HELLO WORLD!

  println(s"Processors: ${Runtime.getRuntime.availableProcessors}")


  println((1 to 10).toList.grouped(3).toList)
  // res16: List[List[Int]] = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9), List(10))

  val result: Future[Int] =
    parallelFoldMap((1 to 1000000).toVector)(identity)

  val future: Future[Int] =
    parallelFoldMap((1 to 1000).toVector)(_ * 1000)


  Thread.sleep(1000)
//  Await.result(result, 1.second)
  println(result)
  // res19: Int = 1784293664

  println(future)
  // res3: Int = 500500000

}
