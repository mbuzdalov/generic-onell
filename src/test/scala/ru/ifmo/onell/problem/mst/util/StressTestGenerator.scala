package ru.ifmo.onell.problem.mst.util

import java.util.Random
import java.util.concurrent.ThreadLocalRandom

object StressTestGenerator {
  def stress(lhs: DynamicGraph.Factory, rhs: DynamicGraph.Factory, nVertices: Int, rng: Random, qLimit: Int): Unit = {
    val allEdges = for {
      b <- 0 until nVertices
      a <- 0 until b
    } yield new DynamicGraph.Edge {
      override val id: Int = b * (b - 1) / 2 + a
      override val vertexA: Int = a
      override val vertexB: Int = b
    }

    assert(allEdges.map(_.id).distinct.size == allEdges.size)
    assert(allEdges.map(_.id).max == nVertices * (nVertices - 1) / 2 - 1)

    val lg = lhs.newEmptyGraph(nVertices, allEdges)
    val rg = rhs.newEmptyGraph(nVertices, allEdges)
    var query = 0L
    while (lg.nConnectivityComponents == rg.nConnectivityComponents && query < qLimit) {
      query += 1
      val id = rng.nextInt(allEdges.size)
      val invProbAdd = if (lg.nConnectivityComponents == 1) 20 else 2
      if (rng.nextInt(invProbAdd) == 0) {
        lg.addEdge(allEdges(id))
        rg.addEdge(allEdges(id))
      } else {
        lg.removeEdge(allEdges(id))
        rg.removeEdge(allEdges(id))
      }
    }
    if (query < qLimit) {
      println(s"Query $query failed. Components are: lhs=${lg.nConnectivityComponents}, rhs=${rg.nConnectivityComponents}")
    } else {
      println("Everything seems OK")
    }
  }

  def main(args: Array[String]): Unit = {
    val seed = ThreadLocalRandom.current().nextLong()
    val nVertices = 100
    println(s"nVertices = $nVertices, seed: $seed")
    stress(NaiveDynamicGraph, AksenovDynamicGraphWrapper, nVertices, new Random(seed), 10000000)
  }
}
