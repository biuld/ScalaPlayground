package com.github.biuld

import com.github.biuld.graph.{DefaultEdge, Edge, Graph}
import org.junit.Test
import scala.collection.parallel.CollectionConverters._

class GraphTest {

  @Test
  def test(): Unit = {
    val edgeSet = Set(Edge("程设", "Java"), Edge("Java", "计组"), Edge("计组", "数据结构"), Edge("数据结构", "Java"))
    val edgeSet2 = Set(Edge("程设", "Java"), Edge("Java", "计组"), Edge("计组", "数据结构"), Edge("Java", "数据结构"))
    val edgeSet3 = Set(Edge("语文", "数学"), Edge("1", "2"), Edge("0", "2"), Edge("2", "3") )

    val graph = Graph[String, DefaultEdge[String]](edgeSet)
    val graph2 = Graph[String, DefaultEdge[String]](edgeSet2)
    val graph3 = Graph[String, DefaultEdge[String]](edgeSet3)

    Graph.topoSort(graph2).foreach(println)
    println()
    Graph.dfs(graph).foreach(println)
    println()
    Graph.bfs(graph).foreach(println)
    println()
    Graph.dfs(graph2).foreach(println)
    println()
    Graph.bfs(graph2).foreach(println)
    println()
    Graph.topoSortAll(graph3).foreach(println)
    println()
    Graph.topoSortAllNeo(graph3).foreach(println)
  }
}
