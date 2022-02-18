package com.breezzo

import cats.kernel.Monoid
import cats.{ Apply, Eq, Functor, Monad, MonoidK }

object EduTestCatsApp extends App {

  type Error[V] = Either[String, V]

  def applyPure() = {
    println(pure[List, Int](10))
  }

  def compose() = {
    val listOpt = Functor[List] compose Functor[Error]
    val list    = List(Right(1), Left("error"), Right(2))
    val mapped  = listOpt.map(list)(_ * 2)
    println(mapped)
  }

  def lift() = {
    val hash  = Functor[List].lift[String, Int](_.hashCode)
    val hash2 = Functor[Option].lift[String, Int](_.hashCode)
    println(hash(List("asd", "123", "123")))
    println(hash2(Some("123")))
    println(hash2(None))
  }

  def map2[F[_], A0, A1, Z](tt: Tuple2[F[A0], F[A1]], ff: (A0, A1) => Z)(implicit functor: Apply[F]): F[Z] = {
    val fTuple: F[(A0, A1)] = functor.product(tt._1, tt._2)
    functor.map(fTuple) { case (a, b) => ff(a, b) }
  }

  def map4[F[_], A0, A1, A2, A3, Z](
    a0: F[A0],
    a1: F[A1],
    a2: F[A2],
    a3: F[A3]
  )(
    ff: (A0, A1, A2, A3) => Z
  )(implicit apply: Apply[F]
  ): F[Z] = {
    apply.map(apply.product(a0, apply.product(a1, apply.product(a2, a3)))) { case (a, (b, (c, d))) =>
      ff(a, b, c, d)
    }
  }

  def pure[F[_], A](x: A)(implicit m: Monoid[F[A]], apply: Apply[F]): F[A] = {
    apply.map(m.empty) { _ => x }
  }

  def flatten[F[_], A](ff: F[F[A]])(implicit fa: Monad[F]): F[A] = {
    fa.flatMap(ff)(x => x)
  }

  def isEmpty[A: Monoid: Eq](a: A): Boolean = {
    implicitly[Monoid[A]].isEmpty(a)
  }

  def fold[F[_], A](ff: Iterable[F[A]])(implicit mk: MonoidK[F]): F[A] = {
    ff.foldLeft(mk.empty[A]) { case (p, x) =>
      mk.combineK(p, x)
    }
  }
}
