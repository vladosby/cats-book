package com.book.cats

import cats.data.Reader
import cats.syntax.applicative._ // for pure

object ReaderExample extends App {

  case class Cat(name: String, favoriteFood: String)

  // defined class Cat
  val catName: Reader[Cat, String] =
    Reader(cat => cat.name)
  // catName: cats.data.Reader[Cat,String] = Kleisli(<function1>)

  println(catName.run(Cat("Garfield", "lasagne")))

  val greetKitty: Reader[Cat, String] =
    catName.map(name => s"Hello ${name}")

  println(greetKitty.run(Cat("Heathcliff", "junk food")))
  // res1: cats.Id[String] = Hello Heathcliff

  val feedKitty: Reader[Cat, String] =
    Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

  val greetAndFeed: Reader[Cat, String] =
    for {
      greet <- greetKitty
      feed <- feedKitty
    } yield s"$greet. $feed."

  println(greetAndFeed(Cat("Garfield", "lasagne")))
  // res3: cats.Id[String] = Hello Garfield. Have a nice bowl of lasagne

  println(greetAndFeed(Cat("Heathcliff", "junk food")))

  // res4: cats.Id[String] = Hello Heathcliff. Have a nice bowl of junk food.


  case class Db(
                 usernames: Map[Int, String],
                 passwords: Map[String, String]
               )


  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(
                     username: String,
                     password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(
                  userId: Int,
                  password: String): DbReader[Boolean] =
    for {
      username <- findUsername(userId)
      valid <- username.map(username => checkPassword(username, password)).getOrElse(false.pure[DbReader])
    } yield valid


  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret")
  val db = Db(users, passwords)
  println(checkLogin(1, "zerocool").run(db))
  // res10: cats.Id[Boolean] = true
  println(checkLogin(4, "davinci").run(db))
  // res11: cats.Id[Boolean] = false
}
