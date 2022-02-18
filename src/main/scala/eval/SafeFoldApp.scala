package com.breezzo
package eval

import cats.Eval

object SafeFoldApp extends App {
  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail =>
        fn(head, foldRight(tail, acc)(fn))
      case Nil          =>
        acc
    }

  def safeFoldRight[A, B](as: List[A], acc: B)(f: (A, B) => B): Eval[B] = {
    as match {
      case head :: tail =>
        Eval.defer {
          safeFoldRight(tail, acc)(f)
            .map(b => f(head, b))
        }
      case Nil          => Eval.now(acc)
    }
  }

  println(safeFoldRight(List.fill(10000)(1), 0)(_ + _).value)
}
