package ru.ifmo.onell.problem.mst.util

class AksenovDynamicGraphWrapper(nVertices: Int, edges: Seq[DynamicGraph.Edge]) extends DynamicGraph:
  private val impl = AksenovDynamicGraph(nVertices, edges.toArray)
  override def nConnectivityComponents: Int = impl.numberOfCC()
  override def addEdge(e: DynamicGraph.Edge): Unit = impl.addEdge(e)
  override def removeEdge(e: DynamicGraph.Edge): Unit = impl.removeEdge(e)

object AksenovDynamicGraphWrapper extends DynamicGraph.Factory:
  override def newEmptyGraph(nVertices: Int, edges: Seq[DynamicGraph.Edge]): DynamicGraph =
    AksenovDynamicGraphWrapper(nVertices, edges)
