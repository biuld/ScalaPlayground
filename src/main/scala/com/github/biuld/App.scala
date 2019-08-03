package com.github.biuld

import com.github.biuld.largeIntCal.{LinkedList, ListNode}

/**
 * Hello world!
 *
 */
object App {
    def main(args: Array[String]): Unit = {
        val a = LinkedList(5)
        val b = LinkedList(6)

        println(a-b)
    }
}
