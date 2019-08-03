package com.github.biuld

import com.github.biuld.largeIntCal.LinkedList
import org.junit.Test

import scala.util.Random

@Test
class largeIntCalTest {

    @Test
    def test(): Unit = {
        wrapper(subtraction_less_than_10, "10以内减法")
        wrapper(subtraction_less_than_100, "100以内减法")
        wrapper(subtraction_less_than_1000, "1000以内减法")

        wrapper(addition_less_than_10, "10以内加法")
        wrapper(addition_less_than_100, "100以内加法")
        wrapper(addition_less_than_1000, "1000以内加法")
    }

    def subtraction_less_than_10(): Unit = {
        for (i <- (0 until 10))
            compute(11, operation = false)
    }

    def subtraction_less_than_100(): Unit = {
        for (i <- (0 until 10))
            compute(101, operation = false)
    }

    def subtraction_less_than_1000(): Unit = {
        for (i <- (0 until 10))
            compute(10001, operation = false)
    }

    def addition_less_than_10(): Unit = {
        for (i <- (0 until 10))
            compute(11, operation = true)
    }

    def addition_less_than_100(): Unit = {
        for (i <- (0 until 10))
            compute(101, operation = true)
    }

    def addition_less_than_1000(): Unit = {
        for (i <- (0 until 10))
            compute(1001, operation = true)
    }

    def wrapper(fn: => () => Unit, msg: String): Unit = {
        println(msg)
        println("==================================")
        fn()
        println("----------------------------------")
    }

    def compute(max: Int, operation: Boolean): Unit = {

        val a = Random.nextInt(max)
        val b = Random.nextInt(max)

        val result = if (operation) (LinkedList(a) + LinkedList(b)).toInt else (LinkedList(a) - LinkedList(b)).toInt
        val expect = if (operation) a + b else a - b
        val sign = if (operation) "+" else "-"

        println(s"$a$sign$b=$expect")
        assert(result == expect, s"got $result, expecting $expect")
    }

}
