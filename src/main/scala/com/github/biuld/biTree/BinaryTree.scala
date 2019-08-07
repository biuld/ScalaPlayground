package com.github.biuld.biTree

import scala.collection.mutable

trait BinaryTree {
    def incl(x: Int): BinaryTree

    def contains(x: Int): Boolean

    def foreach(fn: => BinaryTree => Unit): Unit = {
        BinaryTree.inOrder(this, fn)
    }
}

object BinaryTree {

    def apply(elems: Array[Int]): BinaryTree = {
        var root: BinaryTree = Empty
        elems.foreach(elem => root = root incl elem)
        root
    }

    def height(tree: BinaryTree): Int = {
        tree match {
            case Empty => 0
            case nonEmpty: TreeNode => math.max(height(nonEmpty.left), height(nonEmpty.right)) + 1
        }
    }

    def stringify(style: Int, tree: BinaryTree): String = {

        def levelOrderStr: String = {
            val str = new mutable.StringBuilder
            BinaryTree.levelOrder(tree, {
                case Empty => str append Empty.toString
                case nonEmpty: TreeNode => str append nonEmpty.elem
            })
            str.mkString(",")
        }

        tree match {
            case Empty => Empty.toString
            case nonEmpty: TreeNode => {
                style match {
                    case 0 => "(" + nonEmpty.elem + "." + stringify(0, nonEmpty.left) + "." + stringify(0, nonEmpty.right) + ")"
                    case 1 => "(" + stringify(1, nonEmpty.left) + "." + nonEmpty.elem + "." + stringify(1, nonEmpty.right) + ")"
                    case 2 => "(" + stringify(2, nonEmpty.left) + "." + stringify(2, nonEmpty.right) + "." + nonEmpty.elem + ")"
                    case 3 => "(" + levelOrderStr + ")"
                }
            }
        }
    }

    def inOrder(tree: BinaryTree, fn: => BinaryTree => Unit): Unit = {
        tree match {
            case node: TreeNode => {
                inOrder(node.left, fn)
                fn(node)
                inOrder(node.right, fn)
            }
            case Empty => fn(Empty)
        }
    }

    def preOrder(tree: BinaryTree, fn: => BinaryTree => Unit): Unit = {
        tree match {
            case node: TreeNode => {
                fn(node)
                preOrder(node.left, fn)
                preOrder(node.right, fn)
            }
            case Empty => fn(Empty)
        }
    }

    def postOrder(tree: BinaryTree, fn: => BinaryTree => Unit): Unit = {
        tree match {
            case node: TreeNode => {
                postOrder(node.left, fn)
                postOrder(node.right, fn)
                fn(node)
            }
            case Empty => fn(Empty)
        }
    }

    def levelOrder(tree: BinaryTree, fn: => BinaryTree => Unit): Unit = {
        val queue = new mutable.Queue[BinaryTree]()
        queue += tree

        while (queue.nonEmpty) {
            val root = queue.dequeue()
            root match {
                case Empty => fn(Empty)
                case nonEmpty: TreeNode => {
                    fn(nonEmpty)
                    queue += nonEmpty.left
                    queue += nonEmpty.right
                }
            }
        }
    }
}