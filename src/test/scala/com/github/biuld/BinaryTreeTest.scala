package com.github.biuld

import com.github.biuld.biTree.{BinaryTree, Empty, TreeNode}
import org.junit.jupiter.api.{Assertions, DisplayName, Test}

class BinaryTreeTest {

  val tree: BinaryTree = BinaryTree(Array(1, 3, 2, 4, 5))

  val fn: BinaryTree => Unit = {
    case Empty => print(Empty)
    case nonEmpty: TreeNode => print(nonEmpty.elem)
  }

  @DisplayName("stringify")
  @Test
  def stringify(): Unit = {
    Array(0, 1, 2, 3).foreach(style =>
      println(BinaryTree.stringify(style, tree)))
  }

  @DisplayName("height")
  @Test
  def height(): Unit = {
    Assertions.assertEquals(4, BinaryTree.height(tree))
  }

  @DisplayName("in order traverse")
  @Test
  def inOrder(): Unit = {
    BinaryTree.inOrder(tree, fn)
  }

  @DisplayName("pre order traverse")
  @Test
  def preOrder(): Unit = {
    BinaryTree.preOrder(tree, fn)
  }

  @DisplayName("post order traverse")
  @Test
  def postOrder(): Unit = {
    BinaryTree.postOrder(tree, fn)
  }

  @DisplayName("level order traverse")
  @Test
  def levelOrder(): Unit = {
    BinaryTree.levelOrder(tree, fn)
  }
}
