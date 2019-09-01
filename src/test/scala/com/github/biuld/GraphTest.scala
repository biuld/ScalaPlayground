package com.github.biuld

import com.github.biuld.graph.Graph
import org.junit.Test

class GraphTest {

  @Test
  def test(): Unit = {
    val edgeSet = Set(("程设" -> "Java"), ("Java" -> "计组"), ("计组" -> "数据结构"), ("数据结构" -> "Java"))
    val edgeSet2 = Set(("程设" -> "Java"), ("Java" -> "计组"), ("计组" -> "数据结构"), ("Java" -> "数据结构"))

    val graph = Graph(edgeSet)
    val graph2 = Graph(edgeSet2)
    Graph.topoSort(graph).foreach(println)
    println()
    Graph.topoSort(graph2).foreach(println)
    println()
    Graph.dfs(graph).foreach(println)
    println()
    Graph.bfs(graph).foreach(println)
    println()
    Graph.dfs(graph2).foreach(println)
    println()
    Graph.bfs(graph2).foreach(println)
  }
}
