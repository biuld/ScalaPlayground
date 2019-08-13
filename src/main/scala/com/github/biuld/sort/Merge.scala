package com.github.biuld.sort

import scala.annotation.tailrec

object Merge {

  def mergeSort(xs: List[Int]): List[Int] = {

    @tailrec
    def merge(left: List[Int],
              right: List[Int],
              acc: List[Int] = Nil): List[Int] = {
      (left, right) match {
        case (Nil, _) => acc ::: right
        case (_, Nil) => acc ::: left
        case (lh :: lt, rh :: rt) =>
          if (lh < rh)
            merge(lt, right, acc appended lh)
          else
            merge(rt, left, acc appended rh)
      }
    }

    def sort(xs: List[Int]): List[Int] = {

      val n = xs.length / 2

      if (n == 0)
        return xs

      val (left, right) = xs splitAt n

      merge(sort(left), sort(right))
    }

    sort(xs)
  }
}
