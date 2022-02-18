package com.breezzo
package interpreter

import cats.implicits.catsSyntaxApplicativeId

object CalcInterpreterApp extends App {
  import cats.data.State
  import cats.data.State._
  import cats.instances.int

  type CalcState[A] = State[List[Int], A]

  def op(sym: String): (Int, Int) => Int = sym match {
    case "+" => _ + _
    case "-" => _ - _
    case "*" => _ * _
    case "/" => _ / _
  }

  def operator(f: (Int, Int) => Int): CalcState[Int] = State[List[Int], Int] { case b :: a :: tail =>
    val res = f(a, b)
    (res +: tail, res)
  }

  def operand(sym: String): CalcState[Int] = State[List[Int], Int] { stack =>
    val n = sym.toInt
    (n +: stack, n)
  }

  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
    case _   => operand(sym)
  }

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (a, b) =>
      a.flatMap(_ => evalOne(b))
    }


  def evalInput(symbols: String): Int = {
    evalAll(symbols.split(" ").toList).runEmptyA.value
  }

  val result = evalInput("1 2 + 3 *")

  println(result)
}
