package com.book.cats

import cats.data.Writer
import cats.instances.vector._
import cats.syntax.applicative._ // for pure
import cats.syntax.writer._ // for tell
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object WriterMain extends App {
  println(Writer(Vector(
    "It was the best of times",
    "it was the worst of times"
  ), 1859))
  // res0: cats.data.WriterT[cats.Id,scala.collection.immutable.Vector[
  //  String],Int] = WriterT((Vector(It was the best of times, it was
  //    the worst of times),1859))

  type Logged[A] = Writer[Vector[String], A]

  println(123.pure[Logged])
  // res2: Logged[Int] = WriterT((Vector(),123))

  val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b
  // writer1: cats.data.WriterT[cats.Id,Vector[String],Int] = WriterT((
  //  Vector(a, b, c, x, y, z),42))
  println(writer1.run)
  // res4: cats.Id[(Vector[String], Int)] = (Vector(a, b, c, x, y, z)
  //  ,42)


  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Logged[Int] = {
    for {
      ans <- if(n == 0) {
        1.pure[Logged]
      } else {
        slowly(factorial(n - 1).map(_ * n))
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }

  val Vector((logA, ansA), (logB, ansB)) =
    Await.result(Future.sequence(Vector(
      Future(factorial(3).run),
      Future(factorial(5).run)
    )), 5.seconds)

  println(logA)
  println(logB)

}
