package com.github.biuld.sort

import scala.annotation.tailrec

object Swap {

  def bubbleSort(source: Array[Int]): Array[Int] = {

    @tailrec
    def sort(xs: Array[Int], end: Int): Unit = {

      if (end == 0)
        return

      for (i <- 0 until end)
        if (xs(i) > xs(i + 1)) {
          val temp = xs(i)
          xs(i) = xs(i + 1)
          xs(i + 1) = temp
        }

      sort(xs, end - 1)
    }

    sort(source, source.length - 1)
    source
  }

  @tailrec
  def bubbleSort(xs: List[Int], acc: List[Int] = Nil): List[Int] = {

    @tailrec
    def bubble(xs: List[Int], acc: List[Int] = Nil): List[Int] = xs match {
      case Nil => acc
      case head :: Nil => acc appended head
      case first :: second :: rest =>
        if (first < second)
          bubble(second :: rest, acc appended first)
        else
          bubble(first :: rest, acc appended second)
    }

    xs match {
      case Nil => Nil
      case head :: Nil => head :: acc
      case _ =>
        val (init, last) = bubble(xs) splitAt (xs.length - 1)
        bubbleSort(init, last ::: acc)
    }
  }

  def quickSortAlt(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case head :: tail =>
      val (left, right) = tail partition (_ < head)
      quickSortAlt(left) ::: (head :: quickSortAlt(right))
  }

  /**
   * a uglier version, but tail-recursive(the one and only that I was able to find on Google)
   *
   * @param xs the list to be sorted
   * @return
   */
  def quickSort(xs: List[Int]): List[Int] = {
    @tailrec
    def sort(todo: List[List[Int]], done: List[Int]): List[Int] = todo match {
      case Nil => done // terminal
      case xs :: rest =>
        xs match {
          case Nil => sort(rest, done) // terminal
          case head :: tail =>
            val (ls, rs) = tail partition (_ > head) // head is in rs
            (ls, rs) match {
              case (Nil, Nil) =>
                sort(rest, head :: done) //terminal, yield List(head)
              case (Nil, _) =>
                sort(rs :: rest, head :: done) // head is the largest
              case (_, _) => sort(ls :: List(head) :: rs :: rest, done)
            }
        }
    }

    sort(List(xs), Nil)
  }
}
