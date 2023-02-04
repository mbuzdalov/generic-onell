package ru.ifmo.onell.problem

import ru.ifmo.onell.Fitness
import ru.ifmo.onell.util.{OrderedSet, Permutation}

class OneMaxPerm(val problemSize: Int)
  extends Fitness[Permutation, Int, Long]
{
  override def worstFitness: Int = -1
  override def compare(lhs: Int, rhs: Int): Int = lhs - rhs
  override def isOptimalFitness(fitness: Int): Boolean = fitness == problemSize
  override def numberOfChanges: Long = problemSize * (problemSize - 1L) / 2
  override def changeIndexTypeToLong(st: Long): Long = st

  override def evaluate(individual: Permutation): Int = {
    var i, rv = 0
    val size = individual.size
    while (i < size) {
      if (individual(i) == i) rv += 1
      i += 1
    }
    rv
  }

  override def applyDelta(ind: Permutation, delta: OrderedSet[Long], currentFitness: Int): Int = {
    var i = 0
    var newFitness = currentFitness
    while (i < delta.size) {
      val di = delta(i)
      val j1 = OneMaxPerm.getBigger(di)
      val j0 = OneMaxPerm.getSmaller(di, j1)

      if (ind(j0) == j0) newFitness -= 1
      if (ind(j1) == j1) newFitness -= 1
      ind.swap(j0, j1)
      if (ind(j0) == j0) newFitness += 1
      if (ind(j1) == j1) newFitness += 1
      i += 1
    }
    newFitness
  }

  override def unapplyDelta(ind: Permutation, delta: OrderedSet[Long]): Unit = {
    var i = delta.size - 1
    while (i >= 0) {
      val di = delta(i)
      val j1 = OneMaxPerm.getBigger(di)
      val j0 = OneMaxPerm.getSmaller(di, j1)
      ind.swap(j0, j1)
      i -= 1
    }
  }

  override def fillDelta(from: Permutation, to: Permutation, destination: OrderedSet[Long]): Unit =
    throw new UnsupportedOperationException("Finding differences between permutations is not yet supported")
}

object OneMaxPerm {
  def getBigger(change: Long): Int = ((1 + math.sqrt(1 + 8.0 * change)) / 2).toInt
  def getSmaller(change: Long, bigger: Int): Int = (change - bigger * (bigger - 1L) / 2).toInt
}
