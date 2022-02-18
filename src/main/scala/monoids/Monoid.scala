package com.breezzo
package monoids

trait Semigroup[A] {
  def combine(a: A, b: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit m: Monoid[A]): Monoid[A] = m
}

object MonoidLaws {
  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean = {
    m.combine(x, m.combine(y, z)) ==
      m.combine(m.combine(x, y), z)
  }

  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) &&
    (m.combine(m.empty, x) == x)
  }

  def test[A: Monoid](x: A, y: A, z: A): Boolean =
    associativeLaw(x, y, z) && identityLaw(x) && identityLaw(y) && identityLaw(z)
}

object MonoidInstances {
  implicit val booleanMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false

    override def combine(a: Boolean, b: Boolean): Boolean = a || b
  }

  implicit def setMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    override def empty: Set[A] = Set.empty

    override def combine(a: Set[A], b: Set[A]): Set[A] = a ++ b
  }

  implicit def intMonoid: Monoid[Int] = new Monoid[Int] {
    override def empty: Int = 0

    override def combine(a: Int, b: Int): Int = a + b
  }
}
