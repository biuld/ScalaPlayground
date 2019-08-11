package com.github.biuld

import com.github.biuld.sort.{Insertion, Merge, Selection, Swap}
import org.junit.Test

import scala.util.Random

@Test
class sortingTest {

    val generator: IndexedSeq[Int] = for (i <- 1 to 100) yield Random.nextInt(100)

    @Test
    def test(): Unit = {
        insertionSortTest()
        selectionSortTest()
        swapSortTest()
        mergeSortTest()
    }

    private def insertionSortTest(): Unit = {

        val unSorted = generator.toList
        val expect = unSorted.sorted.mkString(",")

        assert(Insertion.insertionSort(unSorted.toArray).mkString(",") == expect)
        assert(Insertion.shellSort(unSorted.toArray).mkString(",") == expect)
        println("insertion sort test OK!")
    }

    private def selectionSortTest(): Unit = {

        val unSorted = generator.toList
        val expect = unSorted.sorted.mkString(",")

        assert(Selection.selectionSort(unSorted.toArray).mkString(",") == expect)
        assert(Selection.heapSort(unSorted.toArray).mkString(",") == expect)
        println("selection sort test OK!")
    }

    private def swapSortTest(): Unit = {

        val unSorted = generator.toList
        val expect = unSorted.sorted.mkString(",")

        assert(Swap.bubbleSort(unSorted.toArray).mkString(",") == expect)
        assert(Swap.quickSort(unSorted.toArray).mkString(",") == expect)
        println("swap sort test OK!")
    }

    private def mergeSortTest(): Unit = {

        val unSorted = generator.toList
        val expect = unSorted.sorted.mkString(",")

        assert(Merge.mergeSort(unSorted).mkString(",") == expect)
        println("merge sort test OK!")
    }
}
