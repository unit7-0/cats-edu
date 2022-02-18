package com.breezzo

import cats.data.State

object CatsState extends App {

  val st = State[List[Int], Int] { state =>
    (state, state.sum)
  }

  def append(l: List[Int], x: Int): (List[Int], Int) = (l :+ x, l.sum + x)

  val value = st
    .map(_ + 11)
    .transform(append)
    .transform(append)
    .transform(append)
    .transform(append)
    .runEmpty
    .value

  println(value)
}
