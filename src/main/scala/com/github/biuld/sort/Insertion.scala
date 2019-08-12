package com.github.biuld.sort

import scala.annotation.tailrec

object Insertion {

    def insertionSort(source: Array[Int]): Array[Int] = {
        for (i <- 1 until source.length)
            for (j <- (1 to i).reverse)
                if (source(j) < source(j - 1)) {
                    val temp = source(j)
                    source(j) = source(j - 1)
                    source(j - 1) = temp
                }
        source
    }

    def insertionSort(xs: List[Int]): List[Int] = {

        @tailrec
        def insert(xs: List[Int], x: Int, acc: List[Int] = Nil): List[Int] = xs match {
            case Nil => acc appended x
            case head :: tail =>
                if (x < head)
                    acc ::: (x :: xs)
                else
                    insert(tail, x, acc appended head)
        }

        xs.foldLeft(List[Int]()) { (acc, elem) => insert(acc, elem) }
    }

    def shellSort(source: Array[Int]): Array[Int] = {

        var gap = source.length / 2

        while (gap >= 1) {
            for (i <- source.indices by gap)
                for (j <- (gap to i by gap).reverse)
                    if (source(j) < source(j - gap)) {
                        val temp = source(j)
                        source(j) = source(j - gap)
                        source(j - gap) = temp
                    }
            gap /= 2
        }

        source
    }
}
