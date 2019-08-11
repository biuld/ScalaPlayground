package com.github.biuld.sort

object Merge {

    def mergeSort(xs: List[Int]): List[Int] = {

        def merge(left: List[Int], right: List[Int]): List[Int] = {
            (left, right) match {
                case (Nil, _) => right
                case (_, Nil) => left
                case (lHead :: lTail, rHead :: rTail) =>
                    if (lHead < rHead)
                        lHead :: merge(lTail, right)
                    else
                        rHead :: merge(rTail, left)
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
