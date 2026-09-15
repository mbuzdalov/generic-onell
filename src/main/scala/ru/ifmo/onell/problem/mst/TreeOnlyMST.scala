package ru.ifmo.onell.problem.mst

import scala.annotation.tailrec
import scala.collection.mutable.HashSet as MuHashSet
import ru.ifmo.onell.problem.mst.TreeOnlyMST.*
import ru.ifmo.onell.problem.mst.util.DynamicGraph
import ru.ifmo.onell.util.{DisjointSet, OrderedSet, Permutation}
import ru.ifmo.onell.{Fitness, HasIndividualOperations}

import java.util.random.RandomGenerator
import scala.compiletime.uninitialized

class TreeOnlyMST(nVertices: Int, edges: IndexedSeq[Edge], factory: DynamicGraph.Factory) 
extends Fitness[Individual, Long, Long] with HasIndividualOperations[Individual]:
  private val (optimalAnswer, nConnectivityComponents) = solveMST(nVertices, edges)
  private val penalty: Long = math.max(1, edges.view.map(e => math.max(0, e.weight.toLong)).sum)

  assert(edges.forall(e => e.vertexA >= 0 && e.vertexB >= 0 && e.vertexA < nVertices && e.vertexB < nVertices),
         "Some of vertex indices are illegal")
  assert(nConnectivityComponents == 1,
         "The edge set is not connected") // maybe add penalty * nConnectivityComponents to the optimal answer?

  private val internalEdges = edges.zipWithIndex.map(p => InternalEdge(p._2, p._1.vertexA, p._1.vertexB, p._1.weight))

  override def evaluate(individual: Individual): Long = individual.fitness()
  override def compare(lhs: Long, rhs: Long): Int = rhs.compareTo(lhs)
  override def worstFitness: Long = penalty * nVertices
  override def problemSize: Int = internalEdges.size
  override def isOptimalFitness(fitness: Long): Boolean = fitness == optimalAnswer

  override def numberOfChanges: Long = (nVertices - 1L) * (internalEdges.size - nVertices + 1)
  override def changeIndexTypeToLong(st: Long): Long = st

  override def applyDelta(ind: Individual, delta: OrderedSet[Long], currentFitness: Long): Long =
    val size = delta.size
    var i = 0
    while i < size do
      ind.flipPair(delta(i))
      i += 1
    ind.fitness()

  override def unapplyDelta(ind: Individual, delta: OrderedSet[Long]): Unit =
    var i = delta.size
    while i > 0 do
      i -= 1
      ind.flipPair(delta(i))

  override def createStorage(problemSize: Int): Individual = Individual(nVertices, internalEdges, penalty, factory)
  override def initializeRandomly(individual: Individual, rng: RandomGenerator): Unit = individual.initializeRandomly(rng)
end TreeOnlyMST

object TreeOnlyMST:
  case class Edge(vertexA: Int, vertexB: Int, weight: Int)
  private case class InternalEdge(id: Int, vertexA: Int, vertexB: Int, weight: Int) extends DynamicGraph.Edge

  @tailrec
  private def newRandomEdge(nVertices: Int, minWeight: Int, maxWeight: Int, rng: RandomGenerator, existing: MuHashSet[(Int, Int)]): Edge =
    val va, vb = rng.nextInt(nVertices)
    if va < vb && !existing.contains(va -> vb) then
      existing.add(va -> vb)
      Edge(vertexA = va, vertexB = vb, weight = rng.nextInt(maxWeight + 1 - minWeight) + minWeight)
    else newRandomEdge(nVertices, minWeight, maxWeight, rng, existing)

  def randomGraph(nVertices: Int, nEdges: Int, minWeight: Int, maxWeight: Int,
                  rng: RandomGenerator, factory: DynamicGraph.Factory): TreeOnlyMST =
    require(nEdges <= nVertices * (nVertices - 1) / 2, "Too many edges: they need to be different")
    val ds = new DisjointSet(nVertices)
    val builder = IndexedSeq.newBuilder[Edge]
    var components = nVertices
    val existing = new MuHashSet[(Int, Int)]
    while components > 1 do
      val e = newRandomEdge(nVertices, minWeight, maxWeight, rng, existing)
      if ds.unite(e.vertexA, e.vertexB) then
        builder += e
        components -= 1

    for _ <- nVertices - 1 until nEdges do
      builder += newRandomEdge(nVertices, minWeight, maxWeight, rng, existing)

    TreeOnlyMST(nVertices, builder.result(), factory)
  end randomGraph
 
  class Individual private[TreeOnlyMST] (nVertices: Int, edges: IndexedSeq[InternalEdge], penaltyForComponent: Long,
                   graphFactory: DynamicGraph.Factory):
    private var edgeOrder: Permutation = uninitialized
    private var chosenWeight: Long = 0
    private val graph = graphFactory.newEmptyGraph(nVertices, edges)

    def initializeRandomly(rng: RandomGenerator): Unit =
      if edgeOrder == null then edgeOrder = Permutation.identity(edges.size)
      Permutation.shuffle(edgeOrder, rng)
      for i <- 0 until nVertices - 1 do
        val e = edges(edgeOrder(i))
        graph.addEdge(e)
        chosenWeight += e.weight

    def flipPair(edgePairIndex: Long): Unit =
      val nChosenEdges = nVertices - 1
      val indexInChosen = (edgePairIndex % nChosenEdges).toInt
      val indexInNonChosen = (edgePairIndex / nChosenEdges).toInt + nChosenEdges
      edgeOrder.swap(indexInChosen, indexInNonChosen)
      val eIn = edges(edgeOrder(indexInChosen))
      val eOut = edges(edgeOrder(indexInNonChosen))
      graph.addEdge(eIn)
      graph.removeEdge(eOut)
      chosenWeight -= eOut.weight
      chosenWeight += eIn.weight

    def fitness(): Long = chosenWeight + penaltyForComponent * (graph.nConnectivityComponents - 1)
  end Individual

  private def solveMST(nVertices: Int, edges: IndexedSeq[Edge]): (Long, Int) =
    val ds = DisjointSet(nVertices)
    val sortedEdges = edges.sortBy(_.weight)
    var weight = 0L
    var components = nVertices
    var i = 0
    while i < sortedEdges.size do
      val e = sortedEdges(i)
      if ds.unite(e.vertexA, e.vertexB) then
        components -= 1
        weight += e.weight
      i += 1
    (weight, components)
end TreeOnlyMST
