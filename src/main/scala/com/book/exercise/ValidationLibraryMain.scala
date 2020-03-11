package com.book.exercise

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
//import cats.implicits._

import cats.syntax.semigroup._ // for |+|
import cats.syntax.apply._ // for mapN
import cats.syntax.validated._
import cats.instances.list._ // for Semigroup

object ValidationLibraryMain extends App {
  val a: Check[List[String], Int] =
    Pure { v =>
      if(v > 2) v.valid[List[String]]
      else List("Must be > 2").invalid[Int]
    }
  val b: Check[List[String], Int] =
    Pure { v =>
      if(v < -2) v.valid[List[String]]
      else List("Must be < -2").invalid[Int]
    }
  val check: Check[List[String], Int] =
    a and b

  println(check(5))
  println(check(0))
}

sealed trait Check[E, A] {
  def and(that: Check[E, A]): Check[E, A] =
    And(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
    this match {
      case Pure(func) =>
        func(a)
      case And(left, right) =>
        (left(a), right(a)).mapN((_, _) => a)
      case Or(left, right) =>
        left(a) match {
          case Valid(a) => Valid(a)
          case Invalid(e1) =>
            right(a) match {
              case Valid(a) => Valid(a)
              case Invalid(e2) => Invalid(e1 |+| e2)
            }
        }
    }
}

final case class And[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]

final case class Or[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]

final case class Pure[E, A](func: A => Validated[E, A]) extends Check[E, A]