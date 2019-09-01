package com.github.biuld

import com.github.biuld.biTree.{BinaryTree, Empty, TreeNode}
import org.junit.Test

class BinaryTreeTest {

  @Test
  def test(): Unit = {
    val tree: BinaryTree = BinaryTree(Array(1, 3, 2, 4, 5))

    Array(0, 1, 2, 3).foreach(style =>
      println(BinaryTree.stringify(style, tree)))

    println(BinaryTree.height(tree))

    val fn: BinaryTree => Unit = {
      case Empty              => print(Empty)
      case nonEmpty: TreeNode => print(nonEmpty.elem)
    }

    BinaryTree.preOrder(tree, fn)
    println()
    BinaryTree.inOrder(tree, fn)
    println()
    BinaryTree.postOrder(tree, fn)
    println()
    BinaryTree.levelOrder(tree, fn)
  }
}
