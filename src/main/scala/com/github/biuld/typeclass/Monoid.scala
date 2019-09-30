package com.github.biuld.typeclass

import scala.annotation.tailrec

trait Monoid[A] {
  def add(x: A, y: A): A

  def unit: A
}

object Monoid {

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    def add(x: String, y: String): String = x concat y

    def unit: String = ""
  }

  implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
    def add(x: Int, y: Int): Int = x + y

    def unit: Int = 0
  }

  def sum[A](xs: List[A])(implicit m: Monoid[A]): A = {

    @tailrec
    def calculate(xs: List[A], acc: A): A = xs match {
      case Nil => m.add(acc, m.unit)
      case head :: tail => calculate(tail, m.add(acc, head))
    }

    calculate(xs, m.unit)
  }
}