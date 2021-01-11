package ru.ifmo.onell.problem.mst.util

abstract class DynamicGraph {
  def nConnectivityComponents: Int
  def addEdge(e: DynamicGraph.Edge): Unit
  def removeEdge(e: DynamicGraph.Edge): Unit
}

object DynamicGraph {
  abstract class Edge {
    def id: Int
    def vertexA: Int
    def vertexB: Int
  }

  trait Factory {
    def newEmptyGraph(nVertices: Int, edges: Seq[Edge]): DynamicGraph
  }
}
