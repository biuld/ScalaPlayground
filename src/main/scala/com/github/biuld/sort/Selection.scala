package com.github.biuld.sort

import com.github.biuld.biTree.{BinaryTree, TreeNode}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Selection {

    def selectionSort(source: Array[Int]): Array[Int] = {
        for (i <- source.indices)
            for (j <- i + 1 until source.length)
                if (source(j) < source(i)) {
                    val temp = source(i)
                    source(i) = source(j)
                    source(j) = temp
                }
        source
    }

    /**
     * This version perfectly shows the concept of selection
     * @param xs  the list to be sorted
     * @param acc the accumulator
     * @return
     */
    @tailrec
    def selectionSort(xs: List[Int], acc: List[Int] = Nil): List[Int] = xs match {
        case Nil => acc
        case _ => selectionSort(xs.filter(_ < xs.max), xs.filter(_ == xs.max) ::: acc)
    }

    def heapSort(source: Array[Int]): Array[Int] = {

        @tailrec
        def heapify(xs: Array[Int], size: Int, rootIndex: Int): Unit = {

            var largest = rootIndex
            val left = 2 * rootIndex + 1
            val right = 2 * rootIndex + 2

            if (left < size && xs(left) > xs(largest))
                largest = left

            if (right < size && xs(right) > xs(largest))
                largest = right

            if (rootIndex != largest) {
                val temp = xs(rootIndex)
                xs(rootIndex) = xs(largest)
                xs(largest) = temp

                heapify(xs, size, largest)
            }
        }

        @tailrec
        def sort(xs: Array[Int], end: Int): Unit = {

            if (end == 0)
                return

            val temp = xs(0)
            xs(0) = xs(end)
            xs(end) = temp

            heapify(xs, end, 0)
            sort(xs, end - 1)
        }

        //build a heap
        for (i <- (0 until source.length / 2).reverse)
            heapify(source, source.length, i)

        sort(source, source.length - 1)

        source
    }


}
