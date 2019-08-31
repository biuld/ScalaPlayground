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
  def topoSort[T](defaultGraph: Graph[T], acc: List[T] = Nil): List[T] = defaultGraph.root match {
    case None => defaultGraph.edgeSet match {
      case _ if defaultGraph.edgeSet.isEmpty => defaultGraph.vertexSet.foldLeft(acc)((acc, vertex) => acc ::: List(vertex))
      case _ => acc
    }
    case Some(vertex) => topoSort(defaultGraph.removeVertex(vertex), acc ::: List(vertex))
  }
}

