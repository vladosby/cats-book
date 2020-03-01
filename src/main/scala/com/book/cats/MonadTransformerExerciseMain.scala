package com.book.cats

import cats.data.EitherT
import cats.instances.future._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}


object MonadTransformerExerciseMain extends App {

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  type Response[A] = EitherT[Future, String, A]

  def getPowerLevel(autobot: String): Response[Int] =
    EitherT.fromOption[Future](powerLevels.get(autobot), s"$autobot is unreachable")

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
    for {
      allyR1 <- getPowerLevel(ally1)
      allyR2 <- getPowerLevel(ally2)
    } yield (allyR1 + allyR2) > 15
  }

  def tacticalReport(ally1: String, ally2: String): String = {
    val resultF = canSpecialMove(ally1, ally2).value
    Await.result(resultF, 1.second) match {
      case Left(msg) =>
        s"Comms error: $msg"
      case Right(true) =>
        s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) =>
        s"$ally1 and $ally2 need a recharge."
    }
  }


  println(getPowerLevel("Jazz"))
  println(getPowerLevel("Jazz12"))
  val specialMove1 = canSpecialMove("Jazz12", "sas")
  val specialMove2 = canSpecialMove("Jazz", "Hot Rod")

  val tr1 = tacticalReport("Jazz", "Bumblebee")
  // res28: String = Jazz and Bumblebee need a recharge.
  val tr2 = tacticalReport("Bumblebee", "Hot Rod")
  // res29: String = Bumblebee and Hot Rod are ready to roll out!
  val tr3 = tacticalReport("Jazz", "Ironhide")
  // res30: String = Comms error: Ironhide unreachable


  Thread.sleep(100)

  println(specialMove1)
  println(specialMove2)

  println(tr1)
  println(tr2)
  println(tr3)




}
