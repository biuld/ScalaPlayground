package com.github.biuld

import com.github.biuld.biTree.{BinaryTree, TreeNode}

/**
 * Hello world!
 *
 */
object App {
    def main(args: Array[String]): Unit = {

        val tree: BinaryTree = BinaryTree(Array(1, 3, 2, 4, 5))

        Array(0, 1, 2, 3).foreach(style => println(BinaryTree.stringify(style, tree)))

        println(BinaryTree.height(tree))
    }
}
