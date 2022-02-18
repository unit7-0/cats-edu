package com.breezzo
package monoids

object MonoidApp extends App {
  import MonoidInstances._

  def laws() = {
    println(MonoidLaws.test(true, true, true))
    println(MonoidLaws.test(false, false, false))
    println(MonoidLaws.test(true, true, false))
    println(MonoidLaws.test(true, false, true))
    println(MonoidLaws.test(false, true, true))
    println(MonoidLaws.test(true, false, false))
    println(MonoidLaws.test(false, true, false))
    println(MonoidLaws.test(false, false, true))

    println(MonoidLaws.test[Set[Int]](Set.empty, Set(1, 2, 3), Set(1)))
    println(MonoidLaws.test[Set[Int]](Set(3, 2, 1), Set(1, 2, 3), Set(1)))
    println(MonoidLaws.test[Set[Int]](Set.empty, Set.empty, Set.empty))
  }
}

object MonoidAdderApp extends App {
  import cats.instances.int._
  import cats.implicits._

  println(add(List(1.some, none[Int], 3.some, 4.some)))

  final case class Order(totalCost: Double, quantity: Double)

  implicit val orderMonoid: cats.Monoid[Order] = cats.Monoid
    .instance[Order](Order(0.0, 0.0), (o1, o2) => Order(o1.totalCost + o2.totalCost, o1.quantity + o2.quantity))

  println(add(List(Order(10, 1), Order(54.5, 10), Order(1.45, 3))))

  def add[A: cats.Monoid](list: List[A]): A = list.foldLeft(cats.Monoid[A].empty)(_ |+| _)
}
