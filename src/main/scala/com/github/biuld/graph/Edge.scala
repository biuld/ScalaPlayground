package com.github.biuld.graph

sealed trait Edge[T] {
  val source: T
  val target: T
}

case class DefaultEdge[T](source: T, target: T) extends Edge[T]

case class WeightedEdge[T, N](source: T, target: T, weight: N)(implicit comparator: Ordering[N]) extends Edge[T] with Ordered[WeightedEdge[T, N]] {
  override def compare(that: WeightedEdge[T, N]): Int = comparator.compare(this.weight, that.weight)
}

object Edge {
  def apply[T](source: T, target: T): DefaultEdge[T] = DefaultEdge(source, target)

  def apply[T, N](source: T, target: T, weight: N)(implicit comparator: Ordering[N]): WeightedEdge[T, N] = WeightedEdge(source, target, weight)
}