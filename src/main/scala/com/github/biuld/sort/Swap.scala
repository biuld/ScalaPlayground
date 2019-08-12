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

    def quickSort(xs: List[Int]): List[Int] = {

        if (xs.length <= 1)
            return xs

        val pivot = xs.head

        quickSort(xs.filter(_ < pivot)) ::: xs.filter(_ == pivot) ::: quickSort(xs.filter(_ > pivot))
    }

    /**
     * a uglier version, but is tail-recursive(the one and only that I was able to find on Google)
     *
     * @param xs         List to be sorted
     * @param lt         comparing predicate
     * @param conversion converts T to Ordered[T] implicitly
     * @tparam T type parameter
     * @return
     */
    def quickSort[T](xs: List[T])(lt: (T, T) => Boolean)(implicit conversion: T => Ordered[T]): List[T] = {
        @tailrec
        def sort(todo: List[List[T]], done: List[T]): List[T] = todo match {
            case Nil => done // terminal
            case xs :: rest => xs match {
                case Nil => sort(rest, done) // terminal
                case head :: tail =>
                    val (ls, rs) = (tail partition (lt(head, _))) // head is in rs
                    (ls, rs) match {
                        case (Nil, Nil) => sort(rest, head :: done) //terminal, yield List(head)
                        case (Nil, _) => sort(rs :: rest, head :: done) // head is the largest
                        case (_, _) => sort(ls :: List(head) :: rs :: rest, done)

                    }
            }
        }

        sort(List(xs), Nil)
    }
}
