package com.github.biuld.biTree

object Empty extends BinaryTree {
  override def incl(x: Int): BinaryTree = new TreeNode(x, Empty, Empty)

  override def contains(x: Int): Boolean = false

  override def toString: String = "_"
}
