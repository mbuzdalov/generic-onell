package ru.ifmo.onell.problem.mst.util

import scala.annotation.tailrec

import ru.ifmo.onell.problem.mst.util.DynamicGraph.Edge
import ru.ifmo.onell.util.DisjointSet

class NaiveDynamicGraph(nVertices: Int, nEdges: Int) extends DynamicGraph:
  private val edges = Array.ofDim[Edge](nEdges)
  private var nCurrentEdges = 0
  private val edgeIndex = Array.fill(nEdges)(-1)
  private val ds = DisjointSet(nVertices)

  @tailrec
  private def joinComponents(eIdx: Int, nComps: Int): Int = if eIdx < 0 then
    ds.clear()
    nComps
  else
    val e = edges(eIdx)
    joinComponents(eIdx - 1, if ds.unite(e.vertexA, e.vertexB) then nComps - 1 else nComps)

  override def nConnectivityComponents: Int = joinComponents(nCurrentEdges - 1, nVertices)

  override def addEdge(e: Edge): Unit =
    val id = e.id
    require(id >= 0 && id < nEdges, "Wrong edge is to be added (id out of bounds)")
    if edgeIndex(id) < 0 then
      edgeIndex(id) = nCurrentEdges
      edges(nCurrentEdges) = e
      nCurrentEdges += 1

  override def removeEdge(e: Edge): Unit =
    val id = e.id
    require(id >= 0 && id < nEdges, "Wrong edge is to be removed (id out of bounds)")
    if edgeIndex(id) >= 0 then
      nCurrentEdges -= 1
      val idx = edgeIndex(id)
      require(edges(idx) eq e, "An edge is to be removed with the same id but wrong reference (should not happen)")
      if idx != nCurrentEdges then
        edges(idx) = edges(nCurrentEdges)
        edgeIndex(edges(idx).id) = idx
      edgeIndex(id) = -1

object NaiveDynamicGraph extends DynamicGraph.Factory:
  override def newEmptyGraph(nVertices: Int, edges: Seq[Edge]): DynamicGraph =
    NaiveDynamicGraph(nVertices, edges.size)
