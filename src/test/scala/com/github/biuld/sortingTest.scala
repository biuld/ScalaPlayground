package com.github.biuld

import com.github.biuld.sort.{Insertion, Selection}
import org.junit.Test

import scala.util.Random

@Test
class sortingTest {

    val generator: IndexedSeq[Int] = for (i <- 1 to 100) yield Random.nextInt(100)

    val unSorted: Array[Int] = generator.toArray

    val expect: String = unSorted.sorted.mkString(",")

    @Test
    def test(): Unit = {
        insertionSortTest()
        selectionSortTest()
    }

    private def insertionSortTest(): Unit = {
        assert(Insertion.insertionSort(unSorted).mkString(",") == expect)
        assert(Insertion.shellSort(unSorted).mkString(",") == expect)
        println("insertion sort test OK!")
    }

    private def selectionSortTest(): Unit = {
        assert(Selection.selectionSort(unSorted).mkString(",") == expect)
        assert(Selection.heapSort(unSorted).mkString(",") == expect)
        println("selection sort test OK!")
    }
}
