package com.github.biuld

import com.github.biuld.sort.{Insertion, Merge, Selection, Swap}
import org.junit.jupiter.api.{Assertions, DisplayName, Test}

import scala.util.Random

class SortingTest {

  val generator: IndexedSeq[Int] = for (i <- 1 to 100) yield Random.nextInt(100)
  val unSorted: List[Int] = generator.toList
  val expect: String = unSorted.sorted.mkString(",")

  @DisplayName("Insertion Sort")
  @Test
  def insertionSort(): Unit = {
    Assertions.assertEquals(expect, Insertion.insertionSort(unSorted.toArray).mkString(","))
    Assertions.assertEquals(expect, Insertion.insertionSort(unSorted).mkString(","))
    Assertions.assertEquals(expect, Insertion.shellSort(unSorted.toArray).mkString(","))
  }

  @DisplayName("Selection Sort")
  @Test
  def selectionSort(): Unit = {
    Assertions.assertEquals(expect, Selection.selectionSort(unSorted.toArray).mkString(","))
    Assertions.assertEquals(expect, Selection.selectionSort(unSorted).mkString(","))
    Assertions.assertEquals(expect, Selection.heapSort(unSorted.toArray).mkString(","))
  }

  @DisplayName("Swap Sort")
  @Test
  def swapSort(): Unit = {
    Assertions.assertEquals(expect, Swap.bubbleSort(unSorted.toArray).mkString(","))
    Assertions.assertEquals(expect, Swap.bubbleSort(unSorted).mkString(","))
    Assertions.assertEquals(expect, Swap.quickSort(unSorted).mkString(","))
    Assertions.assertEquals(expect, Swap.quickSortAlt(unSorted).mkString(","))
  }

  @DisplayName("Merge Sort")
  @Test
  def mergeSort(): Unit = {
    Assertions.assertEquals(expect, Merge.mergeSort(unSorted).mkString(","))
  }
}
