package ru.ifmo.onell.problem.mst.util

class AksenovDynamicGraphWrapper(nVertices: Int) extends DynamicGraph {
  private[this] val impl = new AksenovDynamicGraph(nVertices)
  impl.clear()
  override def nConnectivityComponents: Int = impl.numberOfCC()
  override def addEdge(e: DynamicGraph.Edge): Unit = impl.addEdge(e.vertexA, e.vertexB)
  override def removeEdge(e: DynamicGraph.Edge): Unit = impl.removeEdge(e.vertexA, e.vertexB)
}

object AksenovDynamicGraphWrapper extends DynamicGraph.Factory {
  override def newEmptyGraph(nVertices: Int, edges: Seq[DynamicGraph.Edge]): DynamicGraph =
    new AksenovDynamicGraphWrapper(nVertices)
}
