package com.github.biuld

import com.github.biuld.graph.{DefaultEdge, Edge, Graph}
import org.junit.jupiter.api.{Assertions, DisplayName, Test}

import scala.collection.parallel.CollectionConverters._

class GraphTest {

  val acyclicEdgeSet: Set[DefaultEdge[Int]] = Set(Edge(1, 2), Edge(1, 3), Edge(3, 4), Edge(2, 4))
  val cyclicEdgeSet: Set[DefaultEdge[Int]] = Set(Edge(1, 2), Edge(1, 3), Edge(2, 3), Edge(3, 4), Edge(4, 2))

  val acyclicGraph: Graph[Int, DefaultEdge[Int]] = Graph[Int, DefaultEdge[Int]](acyclicEdgeSet)
  val cyclicGraph: Graph[Int, DefaultEdge[Int]] = Graph[Int, DefaultEdge[Int]](cyclicEdgeSet)

  val topoResults: List[List[Int]] = List(1 :: 2 :: 3 :: 4 :: Nil, 1 :: 3 :: 2 :: 4 :: Nil)
  val dfsResults: List[List[Int]] = List(1 :: 2 :: 4 :: 3 :: Nil, 1 :: 3 :: 4 :: 2 :: Nil)


  @DisplayName("Topological Sort")
  @Test
  def topo(): Unit = {
    Assertions.assertTrue(topoResults contains Graph.topoSort(acyclicGraph))
    Assertions.assertThrows(classOf[IllegalArgumentException], () => Graph.topoSort(cyclicGraph))
  }

  @DisplayName("All Possible Topological Result")
  @Test
  def topoAll(): Unit = {
    Assertions.assertEquals(topoResults, Graph.topoSortAll(acyclicGraph))
  }

  @DisplayName("Enhanced All Possible Topological Result")
  @Test
  def topoAllNeo(): Unit = {
    Assertions.assertEquals(topoResults, Graph.topoSortAllNeo(acyclicGraph))
  }

  @DisplayName("Depth First Search")
  @Test
  def dfs(): Unit = {
    Assertions.assertTrue(dfsResults contains Graph.dfs(acyclicGraph))
  }

  @DisplayName("Breath First Search")
  @Test
  def bfs(): Unit = {
    Assertions.assertTrue(topoResults contains Graph.bfs(acyclicGraph))
  }
}
