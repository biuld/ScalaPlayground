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

    def quickSort(source: Array[Int]): Array[Int] = {

        if (source.length <= 1)
            return source

        val pivot = source.head

        quickSort(source.filter(_ < pivot)) appendedAll source.filter(_ == pivot) appendedAll quickSort(source.filter(_ > pivot))
    }
}
