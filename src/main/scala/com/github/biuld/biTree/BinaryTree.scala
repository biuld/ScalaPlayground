package com.github.biuld.biTree

import scala.collection.mutable

trait BinaryTree {
    def incl(x: Int): BinaryTree

    def contains(x: Int): Boolean
}

object BinaryTree {

    def apply(elems: Array[Int]): BinaryTree = {
        var root: BinaryTree = Empty
        elems.foreach(elem => root = root incl elem)
        root
    }

    def height(target: BinaryTree): Int = {
        target match {
            case Empty => 0
            case nonEmpty: TreeNode => math.max(height(nonEmpty.left), height(nonEmpty.right)) + 1
        }
    }

    def stringify(style: Int, tree: BinaryTree): String = {

        def levelOrder: String = {
            val queue = new mutable.Queue[BinaryTree]()
            queue += tree

            val str = new mutable.StringBuilder()

            while (queue.nonEmpty) {
                val root = queue.dequeue()
                root match {
                    case Empty => str.append("_")
                    case nonEmpty: TreeNode => {
                        str.append(nonEmpty.elem)
                        queue += nonEmpty.left
                        queue += nonEmpty.right
                    }
                }
            }
            str.mkString(",")
        }

        tree match {
            case Empty => Empty.toString
            case nonEmpty: TreeNode => {
                style match {
                    case 0 => "(" + nonEmpty.elem + "." + stringify(0, nonEmpty.left) + "." + stringify(0, nonEmpty.right) + ")"
                    case 1 => "(" + stringify(1, nonEmpty.left) + "." + nonEmpty.elem + "." + stringify(1, nonEmpty.right) + ")"
                    case 2 => "(" + stringify(2, nonEmpty.left) + "." + stringify(2, nonEmpty.right) + "." + nonEmpty.elem + ")"
                    case 3 => levelOrder
                }
            }
        }
    }
}