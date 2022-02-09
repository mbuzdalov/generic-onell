package ru.ifmo.onell.problem

import java.util.concurrent.ThreadLocalRandom

import scala.annotation.tailrec

import ru.ifmo.onell.problem.VertexCoverProblem.Individual
import ru.ifmo.onell.util.OrderedSet
import ru.ifmo.onell.{Fitness, HasIndividualOperations}

class VertexCoverProblem(val nVertices: Int, edges: Seq[(Int, Int)], val optimum: Long)
  extends Fitness[Individual, Long, Int] with HasIndividualOperations[Individual]
{
  private[this] val adjacentLists = VertexCoverProblem.makeAdjacent(nVertices, edges)

  // meta
  override def problemSize: Int = nVertices
  override def numberOfChanges: Int = nVertices
  override def changeIndexTypeToLong(st: Int): Long = st

  // fitness comparison
  override def compare(lhs: Long, rhs: Long): Int = java.lang.Long.compare(rhs, lhs) // minimization
  override def worstFitness: Long = nVertices * (1L + edges.size)
  override def isOptimalFitness(fitness: Long): Boolean = {
    assert(fitness >= optimum)
    fitness == optimum
  }

  // creation
  override def createStorage(problemSize: Int): Individual = new Individual(problemSize, edges.size)
  override def initializeRandomly(individual: Individual, rng: ThreadLocalRandom): Unit = {
    var i = 0
    while (i < nVertices) {
      if (rng.nextBoolean()) individual.flip(i, adjacentLists)
      i += 1
    }
  }

  // fitness evaluation
  override def evaluate(individual: Individual): Long = individual.fitness
  override def applyDelta(ind: Individual, delta: OrderedSet[Int], currentFitness: Long): Long = {
    unapplyDelta(ind, delta)
    ind.fitness
  }
  override def unapplyDelta(ind: Individual, delta: OrderedSet[Int]): Unit = {
    var i = delta.size
    while (i > 0) {
      i -= 1
      ind.flip(delta(i), adjacentLists)
    }
  }

}

object VertexCoverProblem {
  def makePSProblem(k: Int): VertexCoverProblem = {
    val edges = IndexedSeq.newBuilder[(Int, Int)]
    val offset = 2 * (k + 2)
    for (i <- 0 until k + 2) {
      val single = i
      val hub = i + (k + 2)
      for (j <- 0 until k) {
        edges += (hub -> (offset + j))
      }
      edges += single -> hub
    }
    new VertexCoverProblem(3 * k + 4, edges.result(), k + 2)
  }

  def makeBipartiteGraph(a: Int, b: Int): VertexCoverProblem = {
    new VertexCoverProblem(a + b,
                           for (i <- 0 until a; j <- 0 until b) yield (i, a + j),
                           math.min(a, b))
  }

  class Individual(nVertices: Int, nEdges: Int) {
    private[this] val selected = new Array[Boolean](nVertices)
    private[this] var nSelectedVertices, nCoveredEdges: Int = 0

    def fitness: Long = (nEdges - nCoveredEdges).toLong * nVertices + nSelectedVertices

    def flip(vertex: Int, adjacentVertices: Array[Array[Int]]): Unit = {
      val myAdjacent = adjacentVertices(vertex)
      if (selected(vertex)) {
        nSelectedVertices -= 1
        selected(vertex) = false
        deselect(myAdjacent, 0)
      } else {
        nSelectedVertices += 1
        selected(vertex) = true
        select(myAdjacent, 0)
      }
    }

    @tailrec
    private def deselect(array: Array[Int], index: Int): Unit = if (index < array.length) {
      if (!selected(array(index)))
        nCoveredEdges -= 1
      deselect(array, index + 1)
    }

    @tailrec
    private def select(array: Array[Int], index: Int): Unit = if (index < array.length) {
      if (!selected(array(index)))
        nCoveredEdges += 1
      select(array, index + 1)
    }
  }

  private def makeAdjacent(nVertices: Int, edges: Seq[(Int, Int)]): Array[Array[Int]] = {
    val counts = new Array[Int](nVertices)
    for ((a, b) <- edges) {
      counts(a) += 1
      counts(b) += 1
    }
    val result = counts.map(i => new Array[Int](i))
    for ((a, b) <- edges) {
      counts(a) -= 1
      result(a)(counts(a)) = b
      counts(b) -= 1
      result(b)(counts(b)) = a
    }
    result
  }
}
