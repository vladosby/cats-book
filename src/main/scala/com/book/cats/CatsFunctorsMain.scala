package com.book.cats

import scala.language.higherKinds
import cats.Functor
import cats.instances.list._ // for Functor
import cats.instances.option._ // for Functor
import cats.instances.function._ // for Functor
import cats.syntax.functor._ // for map

object CatsFunctorsMain extends App {

  val list1 = List(1, 2, 3)
  // list1: List[Int] = List(1, 2, 3)
  val list2 = Functor[List].map(list1)(_ * 2)
  // list2: List[Int] = List(2, 4, 6)
  println(list2)
  val option1 = Option(123)
  // option1: Option[Int] = Some(123)
  val option2 = Functor[Option].map(option1)(_.toString)
  println(option2)

  val func = (x: Int) => x + 1
  // func: Int => Int = <function1>
  val liftedFunc = Functor[Option].lift(func)
  // liftedFunc: Option[Int] => Option[Int] = cats.Functor$$Lambda$11698

  println(liftedFunc(Option(1)))

  val func1 = (a: Int) => a + 1
  val func2 = (a: Int) => a * 2
  val func3 = (a: Int) => a + "!"
  val func4 = func1.map(func2).map(func3)
  println(func4(123))

  def doMath[F[_]](start: F[Int])
                  (implicit functor: Functor[F]): F[Int] =
    start.map(n => n + 1 * 2)

  println(doMath(Option(20)))
  // res3: Option[Int] = Some(22)
  println(doMath(List(1, 2, 3)))
  // res4: List[Int] = List(3, 4, 5)
}


/*import scala.concurrent.{Future, ExecutionContext}
implicit def futureFunctor(implicit ec: ExecutionContext): Functor[Future] =
new Functor[Future] {
  def map[A, B](value: Future[A])(func: A => B): Future[B] =
  value.map(func)
}

// We write this:
Functor[Future]
// The compiler expands to this first:
Functor[Future](futureFunctor)
// And then to this:
Functor[Future](futureFunctor(executionContext))*/

object Exercise1 extends App {
  sealed trait Tree[+A]

  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)
    def leaf[A](value: A): Tree[A] =
      Leaf(value)
  }

  final case class Branch[A](left: Tree[A], right: Tree[A])
    extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      fa match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      }
  }

//  println(Branch(Leaf(10), Leaf(20)).map(_ * 2)) does not work
  println(Tree.leaf(100).map(_ * 2))
  println(Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2))
}