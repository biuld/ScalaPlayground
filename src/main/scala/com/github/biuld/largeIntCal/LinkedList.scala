package com.github.biuld.largeIntCal

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class LinkedList(_head: ListNode, _sign: Boolean = true) {

    val head: ListNode = _head
    val sign: Boolean = _sign

    def end: ListNode = {
        var pointer = head
        while (pointer.next != null)
            pointer = pointer.next

        pointer
    }

    def length: Int = {
        var length = 1
        var pointer = this.head

        while (pointer.next != null) {
            length += 1
            pointer = pointer.next
        }

        length
    }

    def foreach(x: => ListNode => Unit): Unit = {
        if (this.length == 1)
            x(head)
        else {
            var pointer = head
            while (pointer.next != null) {
                x(pointer)
                pointer = pointer.next
                if (pointer.next == null)
                    x(pointer)
            }
        }
    }

    def +(that: LinkedList): LinkedList = {

        val (x, y) = LinkedList.calibrate(this, that)

        var c: Int = 0
        val head = new ListNode()
        var pointer = head

        for ((a, b) <- x.toIntArray.reverse zip y.toIntArray.reverse) {

            val z = a + b + c match {
                case result if result >= 10 => result - 10
                case result if result < 0 => result + 10
                case result => result
            }

            c = a + b + c match {
                case result if result >= 10 => 1
                case result if result < 0 => -1
                case _ => 0
            }

            pointer.next = new ListNode(z)
            pointer = pointer.next
        }

        if (c != 0) pointer.next = new ListNode(c)
        var result = new LinkedList(head.next)

        if (c == -1) {
            val i = LinkedList(Math.pow(10, result.length - 1).toInt)
            val j = LinkedList(result.toIntArray.tail.mkString.toInt)
            val k = -(i - j)

            result = LinkedList(LinkedList.strip(k.toNodeArray).reverse, k.sign)
        }

        result
    }

    def -(that: LinkedList): LinkedList = {
        this + (-that)
    }

    def unary_- : LinkedList = {
        val result = new ArrayBuffer[ListNode]()
        this.foreach(node => result.append(new ListNode(-node.value)))
        LinkedList(result.toArray, !sign)
    }

    def toIntArray: Array[Int] = {
        val result: ArrayBuffer[Int] = new ArrayBuffer[Int]()
        this.foreach(node => result.append(node.value))
        result.toArray.reverse
    }

    def toInt: Int = {
        var result = 0
        this.toIntArray.foreach(ele => {
            result = result * 10 + ele
        })
        result
    }

    def toNodeArray: Array[ListNode] = {
        val result = new ArrayBuffer[ListNode]()
        this.foreach(node => result.append(new ListNode(node.value)))
        result.toArray.reverse
    }

    def reverse: LinkedList = {
        LinkedList(this.toNodeArray, sign)
    }

    override def clone: LinkedList = {
        LinkedList(this.toNodeArray.reverse, sign)
    }

    override def toString: String = {
        val sign: String = if (this.sign) "+" else "-"
        sign + this.toIntArray.map(_.abs).mkString
    }
}

object LinkedList {

    def apply(value: Int): LinkedList = {

        val valueReStr = value.abs.toString.reverse

        val head = new ListNode()
        var pointer = head

        valueReStr.foreach(digitStr => {
            val node = new ListNode(digitStr.asDigit)
            pointer.next = node
            pointer = pointer.next
        })

        val result = new LinkedList(head.next, value >= 0)

        result
    }

    def apply(nodeArray: Array[ListNode], sign: Boolean): LinkedList = {

        val head = new ListNode()
        var pointer = head

        nodeArray.foreach(node => {
            pointer.next = node
            pointer = pointer.next
        })

        new LinkedList(head.next, sign)
    }

    def unapply(arg: LinkedList): Option[(ListNode, ListNode)] = {
        Some(arg.head, arg.head.next)
    }

    def calibrate(a: LinkedList, b: LinkedList): (LinkedList, LinkedList) = {

        def extend(target: LinkedList, length: Int): LinkedList = {
            val clone = target.clone
            var pointer = clone.end

            for (i <- (0 until length - target.length)) {
                pointer.next = new ListNode()
                pointer = pointer.next
            }
            clone
        }

        a match {
            case a if a.length == b.length => (a, b)
            case a if a.length > b.length => (a, extend(b, a.length))
            case a if a.length < b.length => (extend(a, b.length), b)
        }
    }

    @tailrec
    def strip(nodeArray: Array[ListNode]): Array[ListNode] = {

        if (nodeArray.length == 1)
            return nodeArray

        if (nodeArray.head.value != 0)
            return nodeArray

        strip(nodeArray.tail)
    }
}
