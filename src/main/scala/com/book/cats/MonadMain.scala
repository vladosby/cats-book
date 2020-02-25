package com.book.cats

import scala.language.higherKinds
import cats.{Id, Monad}
import cats.instances.option._
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.flatMap._ // for flatMap

trait Monad2[F[_]] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  def map[A, B](value: F[A])(func: A => B): F[B] = flatMap(value)(func andThen pure)
}


object MonadMain extends App {
  val opt1 = Monad[Option].pure(3)
  // opt1: Option[Int] = Some(3)
  println(opt1)
  val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
  // opt2: Option[Int] = Some(5)
  println(opt2)
  val opt3 = Monad[Option].map(opt2)(a => 100 * a)
  // opt3: Option[Int] = Some(500)
  println(opt3)
  val list1 = Monad[List].pure(3)
  // list1: List[Int] = List(3)
  println(list1)
  val list2 = Monad[List].
    flatMap(List(1, 2, 3))(a => List(a, a * 10))
  // list2: List[Int] = List(1, 10, 2, 20, 3, 30)
  println(list2)
  val list3 = Monad[List].map(list2)(a => a + 123)
  // list3: List[Int] = List(124, 133, 125, 143, 126, 153)
  println(list3)

  def sumSquare[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x * x + y * y

  println(sumSquare(Option(3), Option(4)))
  // res10: Option[Int] = Some(25)
  println(sumSquare(3: Id[Int], 4: Id[Int]))

  println(sumSquare(3: Id[Int], 4: Id[Int]) + 3)

  val a = Monad[Id].pure(3)
  // a: cats.Id[Int] = 3
  val b = Monad[Id].flatMap(a)(_ + 1)
  // b: cats.Id[Int] = 4

  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap
  val result = for {
    x <- a
    y <- b
  } yield x + y
  // res6: cats.Id[Int] = 7

  println(result)
}

object Exercise22 extends App {
  def pure[A](value: A): Id[A] = value

  def map[A, B](initial: Id[A])(func: A => B): Id[B] =
    func(initial)

  def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] =
    func(initial)

  println(pure(123))
  println(map(123)(_ * 2))
  println(flatMap(123)(_ * 2))

  /*  def countPositive(nums: List[Int]) =
      nums.foldLeft(Right(0)) { (accumulator, num) =>
        if(num > 0) {
          accumulator.map(_ + 1)
        } else {
          Left("Negative. Stopping!")
        }
      }

    // <console>:21: error: type mismatch;
    // found : scala.util.Either[Nothing,Int]
    // required: scala.util.Right[Nothing,Int]
    // accumulator.map(_ + 1)
    // ^
    // <console>:23: error: type mismatch;
    // found : scala.util.Left[String,Nothing]
    // required: scala.util.Right[Nothing,Int]
    // Left("Negative. Stopping!")
    //
   */

  import cats.syntax.either._

  def countPositive(nums: List[Int]) =
    nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
      if (num > 0) {
        accumulator.map(_ + 1)
      } else {
        Left("Negative. Stopping!")
      }
    }

  println(countPositive(List(1, 2, 3)))

  println(Either.catchOnly[NumberFormatException]("foo".toInt))
  println(Either.catchNonFatal(sys.error("Badness")))
  println(Either.catchNonFatal(("foo".toInt)))
  println(Either.fromTry(scala.util.Try("foo".toInt)))
  println(Either.fromOption[String, Int](None, "Badness"))
  println(Either.fromOption[String, Int](Some(2), "Badness"))

  println("Error".asLeft[Int].getOrElse(0))
  // res11: Int = 0

  println("Error".asLeft[Int].orElse(2.asRight[String]))
  // res12: Either[String,Int] = Right(2)

  println((-1).asRight[String].ensure("Must be non-negative!")(_ > 0))
  // res13: Either[String,Int] = Left(Must be non-negative!)


  println("error".asLeft[Int].recover {
    case str: String => -1
  })
  // res14: Either[String,Int] = Right(-1)

  println("error".asLeft[Int].recoverWith {
    case str: String => Right(-1)
  })
  // res15: Either[String,Int] = Right(-1)

  println("foo".asLeft[Int].leftMap(_.reverse))
  // res16: Either[String,Int] = Left(oof)
  println(6.asRight[String].bimap(_.reverse, _ * 7))
  // res17: Either[String,Int] = Right(42)
  println("bar".asLeft[Int].bimap(_.reverse, _ * 7))
  // res18: Either[String,Int] = Left(rab)
}