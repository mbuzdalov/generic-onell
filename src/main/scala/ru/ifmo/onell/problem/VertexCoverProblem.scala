package ru.ifmo.onell.problem

import scala.annotation.tailrec
import org.jgrapht.alg.vertexcover.RecursiveExactVCImpl
import org.jgrapht.graph.DefaultUndirectedGraph
import ru.ifmo.onell.problem.VertexCoverProblem.Individual
import ru.ifmo.onell.util.OrderedSet
import ru.ifmo.onell.{Fitness, HasIndividualOperations}

import java.util.Random
import java.util.random.RandomGenerator

class VertexCoverProblem(val nVertices: Int, edges: Seq[(Int, Int)], val optimum: Long)
extends Fitness[Individual, Long, Int] with HasIndividualOperations[Individual]:
  private val adjacentLists = VertexCoverProblem.makeAdjacent(nVertices, edges)

  // meta
  override def problemSize: Int = nVertices
  override def numberOfChanges: Int = nVertices
  override def changeIndexTypeToLong(st: Int): Long = st

  // fitness comparison
  override def compare(lhs: Long, rhs: Long): Int = java.lang.Long.compare(rhs, lhs) // minimization
  override def worstFitness: Long = nVertices * (1L + edges.size)
  override def isOptimalFitness(fitness: Long): Boolean =
    assert(fitness >= optimum)
    fitness == optimum

  // creation
  override def createStorage(problemSize: Int): Individual = Individual(problemSize, edges.size)
  override def initializeRandomly(individual: Individual, rng: RandomGenerator): Unit =
    var i = 0
    while i < nVertices do
      if rng.nextBoolean() then individual.flip(i, adjacentLists)
      i += 1

  // fitness evaluation
  override def evaluate(individual: Individual): Long = individual.fitness
  override def applyDelta(ind: Individual, delta: OrderedSet[Int], currentFitness: Long): Long =
    unapplyDelta(ind, delta)
    ind.fitness

  override def unapplyDelta(ind: Individual, delta: OrderedSet[Int]): Unit =
    var i = delta.size
    while i > 0 do
      i -= 1
      ind.flip(delta(i), adjacentLists)
end VertexCoverProblem

object VertexCoverProblem:
  def makePSProblem(k: Int): VertexCoverProblem =
    val edges = IndexedSeq.newBuilder[(Int, Int)]
    val offset = 2 * (k + 2)
    for i <- 0 until k + 2 do
      val single = i
      val hub = i + (k + 2)
      for j <- 0 until k do edges += (hub -> (offset + j))
      edges += single -> hub
    end for
    VertexCoverProblem(3 * k + 4, edges.result(), k + 2)

  def makeBipartiteGraph(a: Int, b: Int): VertexCoverProblem =
    VertexCoverProblem(a + b,
                       for i <- 0 until a; j <- 0 until b yield (i, a + j),
                       math.min(a, b))

  def makeRandom(v: Int, e: Int, seed: Long): VertexCoverProblem =
    val rng = Random(seed)
    val edges = IndexedSeq.newBuilder[(Int, Int)]
    val graph = DefaultUndirectedGraph[Int, (Int, Int)](classOf[(Int, Int)])
    for i <- 0 until v do graph.addVertex(i)
    while edges.knownSize < e do
      val a, b = rng.nextInt(v)
      if a != b then 
        edges += (a -> b)
        graph.addEdge(a, b, a -> b)
    end while
    VertexCoverProblem(v, edges.result(), RecursiveExactVCImpl(graph).getVertexCover.size())

  class Individual(nVertices: Int, nEdges: Int):
    private val selected = Array.ofDim[Boolean](nVertices)
    private var nSelectedVertices, nCoveredEdges: Int = 0

    def fitness: Long = (nEdges - nCoveredEdges).toLong * nVertices + nSelectedVertices

    def flip(vertex: Int, adjacentVertices: Array[Array[Int]]): Unit =
      val myAdjacent = adjacentVertices(vertex)
      if selected(vertex) then
        nSelectedVertices -= 1
        selected(vertex) = false
        deselect(myAdjacent, 0)
      else
        nSelectedVertices += 1
        selected(vertex) = true
        select(myAdjacent, 0)

    @tailrec
    private def deselect(array: Array[Int], index: Int): Unit = if index < array.length then
      if !selected(array(index)) then nCoveredEdges -= 1
      deselect(array, index + 1)

    @tailrec
    private def select(array: Array[Int], index: Int): Unit = if index < array.length then
      if !selected(array(index)) then nCoveredEdges += 1
      select(array, index + 1)
  end Individual

  private def makeAdjacent(nVertices: Int, edges: Seq[(Int, Int)]): Array[Array[Int]] =
    val counts = Array.ofDim[Int](nVertices)
    for (a, b) <- edges do
      counts(a) += 1
      counts(b) += 1
    val result = counts.map(i => Array.ofDim[Int](i))
    for (a, b) <- edges do
      counts(a) -= 1
      result(a)(counts(a)) = b
      counts(b) -= 1
      result(b)(counts(b)) = a
    result
end VertexCoverProblem
