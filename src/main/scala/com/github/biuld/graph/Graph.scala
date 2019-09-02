package com.github.biuld.graph

import scala.annotation.tailrec

class Graph[V, E <: Edge[V]](_vertexSet: Set[V], _edgeSet: Set[E]) {

  val vertexSet: Set[V] = _vertexSet
  val edgeSet: Set[E] = _edgeSet

  def removeVertex(vertex: V): Graph[V, E] = {
    val edgeSet = this.edgeSet.filter(!_.source.equals(vertex)).filter(!_.target.equals(vertex))
    val vertexSet = this.vertexSet - vertex
    new Graph[V, E](vertexSet, edgeSet)
  }

  def countOutEdges(vertex: V): Int = edgeSet.count(_.source.equals(vertex))

  def countInEdges(vertex: V): Int = edgeSet.count(_.target.equals(vertex))

  def root: Option[V] = vertexSet.find(countInEdges(_) == 0)

  override def toString: String = this.vertexSet + "\n" + this.edgeSet
}

object Graph {

  def apply[V, E <: Edge[V]](edgeSet: Set[E] = Set[E]()): Graph[V, E] = {
    val vertexSet = edgeSet.foldLeft(Set[V]().empty)((acc, elem) => acc incl elem.source incl elem.target)
    new Graph[V, E](vertexSet, edgeSet)
  }

  @tailrec
  def topoSort[V, E <: Edge[V]](graph: Graph[V, E], acc: List[V] = Nil): List[V] = graph.root match {
    case None => graph.edgeSet match {
      case _ if graph.edgeSet.isEmpty => graph.vertexSet.foldLeft(acc)((acc, vertex) => acc ::: List(vertex))
      case _ => acc
    }
    case Some(vertex) => topoSort(graph.removeVertex(vertex), acc ::: List(vertex))
  }

  def dfs[V, E <: Edge[V]](graph: Graph[V, E]): List[V] = {

    @tailrec
    def traverse(start: V, graph: Graph[V, E], acc: List[V] = Nil): List[V] = {

      val next = graph.edgeSet.find(_.source.equals(start)).map(_.target)

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
  def bfs[V, E <: Edge[V]](graph: Graph[V, E], queue: List[V] = Nil, acc: List[V] = Nil): List[V] = queue match {
    case Nil => graph.vertexSet match {
      case _ if graph.vertexSet.isEmpty => acc
      case _ => bfs(graph, graph.vertexSet.head :: Nil)
    }
    case head :: tail =>

      val queue2 = graph.edgeSet
        .filter(_.source.equals(head))
        .map(_.target)
        .foldLeft(tail)((acc, elem) => if (acc contains elem) acc else acc ::: List(elem))

      bfs(graph.removeVertex(head), queue2, acc ::: List(head))
  }

}
