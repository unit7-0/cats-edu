package com.breezzo
package typeclasses

object PrintableApp extends App {

  import printable.PrintableInstances._
  import printable._

  Printable.print("abc")
  Printable.print(112)

  final case class Cat(name: String, age: Int, color: String)

  object Cat {
    implicit val printable: Printable[Cat] = new Printable[Cat] {
      override def format(a: Cat): String = {
        val name  = Printable.format(a.name)
        val age   = Printable.format(a.age)
        val color = Printable.format(a.color)
        s"$name is a $age year-old $color cat."
      }
    }
  }

  Cat("Barsik", 5, "black").print
}

object ShowApp extends App {
  import cats.Show
  import cats.implicits._

  final case class Cat(name: String, age: Int, color: String)

  object Cat {
    implicit val catShow: Show[Cat] = Show.show { cat =>
      val name  = cat.name.show
      val age   = cat.age.show
      val color = cat.color.show
      s"$name is a $age year-old $color cat."
    }
  }

  println(Cat("Barsik", 5, "black").show)
}

object printable {
  sealed trait Printable[A] {
    def format(a: A): String
  }

  object Printable {
    def apply[A: Printable]: Printable[A] = implicitly[Printable[A]]

    def format[A: Printable](a: A): String = Printable[A].format(a)

    def print[A: Printable](a: A): Unit = println(format(a))
  }

  object PrintableInstances {
    implicit val printableString: Printable[String] = new Printable[String] {
      override def format(a: String): String = a
    }

    implicit val printableInt: Printable[Int] = new Printable[Int] {
      override def format(a: Int): String = a.toString
    }

    implicit class PrintableOps[A](a: A) {
      def format(implicit printable: Printable[A]): String = Printable.format(a)
      def print(implicit printable: Printable[A]): Unit    = Printable.print(a)
    }
  }
}

object EqApp extends App {
  import cats.Eq
  import cats.instances.int._
  import cats.instances.string._
  import cats.syntax.eq._
  import cats.syntax.option._

  final case class Cat(name: String, age: Int, color: String)

  object Cat {
    implicit val catEq: Eq[Cat] = Eq.instance[Cat] { (c1, c2) =>
      c1.name === c2.name && c1.age === c2.age && c1.color === c2.color
    }
  }

  val cat1 = Cat("Garfield",   38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  println(cat1 === cat2)
  println(cat1.some === optionCat1)
  println(cat2.some === optionCat2)
  println(optionCat1 === optionCat2)
}
