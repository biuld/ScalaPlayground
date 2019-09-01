package com.github.biuld.graph

import scala.annotation.tailrec

class Graph[T](_vertexSet: Set[T], _edgeSet: Set[(T, T)]) {

  type Edge = (T, T)

  val vertexSet: Set[T] = _vertexSet
  val edgeSet: Set[Edge] = _edgeSet

  def reduce(edgeSet: Set[Edge]): List[T] = edgeSet.foldLeft(List[T]().empty)((acc, elem) => acc ::: List(elem._2))

  def adjacencyList: Map[T, List[T]] = {
    edgeSet.groupBy(elem => elem._1)
      .foldLeft(Map[T, List[T]]().empty)((acc, elem) => acc + (elem._1 -> reduce(elem._2)))
  }

  def removeVertex(vertex: T): Graph[T] = {
    val edgeSet = this.edgeSet.filter(elem => !elem._1.equals(vertex)).filter(elem => !elem._2.equals(vertex))
    val vertexSet = this.vertexSet - vertex
    new Graph[T](vertexSet, edgeSet)
  }

  def countOutEdges(vertex: T): Int = edgeSet.count(elem => elem._1.equals(vertex))

  def countInEdges(vertex: T): Int = edgeSet.count(elem => elem._2.equals(vertex))

  def root: Option[T] = vertexSet.find(elem => countInEdges(elem) == 0)

  override def toString: String = this.vertexSet + "\n" + this.edgeSet
}

object Graph {

  def apply[T](edgeSet: Set[(T, T)] = Set[(T, T)]()): Graph[T] = {
    val vertexSet = edgeSet.foldLeft(Set[T]().empty)((acc, elem) => acc incl elem._1 incl elem._2)
    new Graph[T](vertexSet, edgeSet)
  }

  @tailrec
  def topoSort[T](graph: Graph[T], acc: List[T] = Nil): List[T] = graph.root match {
    case None => graph.edgeSet match {
      case _ if graph.edgeSet.isEmpty => graph.vertexSet.foldLeft(acc)((acc, vertex) => acc ::: List(vertex))
      case _ => acc
    }
    case Some(vertex) => topoSort(graph.removeVertex(vertex), acc ::: List(vertex))
  }

  def dfs[T](graph: Graph[T]): List[T] = {

    @tailrec
    def traverse(start: T, graph: Graph[T], acc: List[T] = Nil): List[T] = {

      val next = graph.edgeSet.find(_._1.equals(start)).map(_._2)

      next match {
        case None =>
          val graph2 = graph.removeVertex(start)
          graph2.vertexSet match {
            case _ if graph2.vertexSet.isEmpty => acc ::: List(start)
            case _ => traverse(graph2.vertexSet.head, graph2, acc ::: List(start))
          }
        case Some(vertex) => traverse(vertex, graph.removeVertex(start), acc ::: List(start))
      }
    }

    traverse(graph.vertexSet.head, graph)
  }

  @tailrec
  def bfs[T](graph: Graph[T], queue: List[T] = Nil, acc: List[T] = Nil): List[T] = queue match {
    case Nil => graph.vertexSet match {
      case _ if graph.vertexSet.isEmpty => acc
      case _ => bfs(graph, graph.vertexSet.head :: Nil)
    }
    case head :: tail =>

      val queue2 = graph.edgeSet
        .filter(_._1.equals(head))
        .map(_._2)
        .foldLeft(tail)((acc, elem) => if (acc contains elem) acc else acc ::: List(elem))

      bfs(graph.removeVertex(head), queue2, acc ::: List(head))
  }

}

