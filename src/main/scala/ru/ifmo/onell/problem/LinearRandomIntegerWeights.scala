package ru.ifmo.onell.problem

import java.util.Random

import ru.ifmo.onell.Fitness
import ru.ifmo.onell.util.{Helpers, OrderedSet}

class LinearRandomIntegerWeights(val problemSize: Int, val maxWeight: Int, randomSeed: Long)
  extends Fitness[Array[Boolean], Long, Int]
{
  private[this] val rng = new Random(randomSeed)
  private[this] val weights = Array.fill(problemSize)(rng.nextInt(maxWeight) + 1)
  private[this] val weightSum = weights.view.map(_.toLong).sum

  override def evaluate(individual: Array[Boolean]): Long = {
    var i = individual.length - 1
    var fitness = 0L
    while (i >= 0) {
      if (individual(i)) {
        fitness += weights(i)
      }
      i -= 1
    }
    fitness
  }

  override def worstFitness: Long = -1
  override def compare(lhs: Long, rhs: Long): Int = java.lang.Long.compare(lhs, rhs)
  override def isOptimalFitness(fitness: Long): Boolean = fitness == weightSum
  override def numberOfChanges: Int = problemSize
  override def changeIndexTypeToLong(st: Int): Long = st

  override def applyDelta(ind: Array[Boolean], delta: OrderedSet[Int], currentFitness: Long): Long = {
    val size = delta.size
    var newFitness = currentFitness
    var i = 0
    while (i < size) {
      val idx = delta(i)
      newFitness += (if (ind(idx)) -weights(idx) else weights(idx))
      ind(idx) ^= true
      i += 1
    }
    newFitness
  }

  override def unapplyDelta(ind: Array[Boolean], delta: OrderedSet[Int]): Unit =
    Helpers.flipEachBit(ind, delta)
  override def fillDelta(from: Array[Boolean], to: Array[Boolean], destination: OrderedSet[Int]): Unit =
    Helpers.findDifferingBits(from, to, destination)

  override def evaluateAssumingDelta(ind: Array[Boolean], delta: OrderedSet[Int], currentFitness: Long): Long = {
    val size = delta.size
    var newFitness = currentFitness
    var i = 0
    while (i < size) {
      val idx = delta(i)
      newFitness += (if (ind(idx)) -weights(idx) else weights(idx))
      i += 1
    }
    newFitness
  }
}
