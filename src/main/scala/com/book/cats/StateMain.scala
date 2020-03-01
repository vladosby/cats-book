package com.book.cats

import cats.data.State
import cats.syntax.applicative._ // for pure

object StateMain extends App {
  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = {
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }
  }

  def operand(num: Int): CalcState[Int] = {
    State[List[Int], Int] { stack =>
      (num :: stack, num)
    }
  }

  def operator(func: (Int, Int) => Int): CalcState[Int] = {
    State[List[Int], Int] {
      case a :: b :: tail =>
        val ans = func(a, b)
        (ans :: tail, ans)
      case _ => sys.error("Fail!")
    }
  }

  def evalAll(input: List[String]): CalcState[Int] = {
    input.foldLeft(0.pure[CalcState])((sum, value) => sum.flatMap(_ => evalOne(value)))
  }

  println(evalOne("42").runA(Nil).value)
  // res3: Int = 42

  val program = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    _ <- evalOne("+")
    _ <- evalOne("2")
    _ <- evalOne("3")
    _ <- evalOne("+")
    ans <- evalOne("+")
  } yield ans

  println(program.runA(Nil).value)
  // res4: Int = 3


  val program2 = evalAll(List("1", "2", "+", "3", "*"))
  // program: CalcState[Int] = cats.data.IndexedStateT@2a6631ab
  println(program2.runA(Nil).value)
  // res6: Int = 9


  val program3 = for {
    _ <- evalAll(List("1", "2", "+"))
    _ <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans
    // program: cats.data.IndexedStateT[cats.Eval,List[Int],List[Int],Int]
//    = cats.data.IndexedStateT@45606aa5
    println(program3.runA(Nil).value)
  // res7: Int = 21

  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value

  println(evalInput("1 2 + 3 4 + *"))
}
