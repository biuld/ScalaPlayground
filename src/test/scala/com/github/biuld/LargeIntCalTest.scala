package com.github.biuld

import com.github.biuld.largeIntCal.LinkedList
import org.junit.jupiter.api.{Assertions, DisplayName, Test}

import scala.util.Random

class LargeIntCalTest {

  @DisplayName("sub <= 10")
  @Test
  def subtraction_less_than_10(): Unit = {
    for (i <- (0 until 10))
      compute(11, operation = false)
  }

  @DisplayName("sub <= 100")
  @Test
  def subtraction_less_than_100(): Unit = {
    for (i <- (0 until 10))
      compute(101, operation = false)
  }

  @DisplayName("sub <= 1000")
  @Test
  def subtraction_less_than_1000(): Unit = {
    for (i <- (0 until 10))
      compute(10001, operation = false)
  }

  @DisplayName("add <= 10")
  @Test
  def addition_less_than_10(): Unit = {
    for (i <- (0 until 10))
      compute(11, operation = true)
  }

  @DisplayName("add <= 100")
  @Test
  def addition_less_than_100(): Unit = {
    for (i <- (0 until 10))
      compute(101, operation = true)
  }

  @DisplayName("add <= 1000")
  @Test
  def addition_less_than_1000(): Unit = {
    for (i <- (0 until 10))
      compute(1001, operation = true)
  }

  private def compute(max: Int, operation: Boolean): Unit = {

    val a = Random.nextInt(max)
    val b = Random.nextInt(max)

    val result =
      if (operation) (LinkedList(a) + LinkedList(b)).toInt
      else (LinkedList(a) - LinkedList(b)).toInt
    val expect = if (operation) a + b else a - b

    Assertions.assertEquals(expect, result)
  }
}
