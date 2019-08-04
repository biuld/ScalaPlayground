package com.github.biuld.biTree

class TreeNode(_elem: Int, _left: BinaryTree, _right: BinaryTree) extends BinaryTree {

    val elem: Int = _elem
    val left: BinaryTree = _left
    val right: BinaryTree = _right

    override def incl(x: Int): BinaryTree = {
        if (x < elem) new TreeNode(elem, left incl x, right)
        else if (x > elem) new TreeNode(elem, left, right incl x)
        else this
    }

    override def contains(x: Int): Boolean = {
        if (x < elem)
            left contains x
        else if (x > elem)
            right contains x
        else true
    }

    override def toString: String = BinaryTree.stringify(1, this)

}
