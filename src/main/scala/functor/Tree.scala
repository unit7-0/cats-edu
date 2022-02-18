package com.breezzo
package functor

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

import cats.Functor

object FunctorInstances {
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = {
      fa match {
        case Branch(left, right) =>
          val leftMapped = map(left)(f)
          val rightMapped = map(right)(f)
          Branch(leftMapped, rightMapped)
        case Leaf(value) => Leaf(f(value))
      }
    }
  }
}

object FunctorApp extends App {
  import FunctorInstances._
  import cats.syntax.functor._

  val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))

  println(tree)
  println(tree.map(_ * 2))
  println(tree.map(_.toString.prepended('0')))
}
