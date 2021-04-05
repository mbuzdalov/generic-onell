package ru.ifmo.onell.problem

import java.util.concurrent.ThreadLocalRandom

import ru.ifmo.onell.util.OrderedSet
import ru.ifmo.onell.{Fitness, HasIndividualOperations}

import RandomKnapsack._

class RandomKnapsack(val problemSize: Int, val capacity: Int, seed: Long)
  extends Fitness[Individual, (Long, Long), Int] with HasIndividualOperations[Individual]
{
  private[this] val rng = new java.util.Random(seed + 1)
  private[this] val weights = Array.fill(problemSize)(rng.nextInt(16) + 5)
  private[this] val values = Array.fill(problemSize)(rng.nextInt(51) + 50)

  override def evaluate(individual: Individual): (Long, Long) = (individual.sumOfWeights, individual.sumOfValues)
  override def compare(lhs: (Long, Long), rhs: (Long, Long)): Int = {
    val constraintL = math.max(0, lhs._1 - capacity)
    val constraintR = math.max(0, rhs._1 - capacity)
    if (constraintL != constraintR)
      constraintR.compare(constraintL)
    else
      lhs._2.compare(rhs._2)
  }

  override val worstFitness: (Long, Long) = (Long.MaxValue, Long.MinValue)
  override def isOptimalFitness(fitness: (Long, Long)): Boolean = false
  override def numberOfChanges: Int = problemSize
  override def changeIndexTypeToLong(st: Int): Long = st
  override def applyDelta(ind: Individual, delta: OrderedSet[Int], currentFitness: (Long, Long)): (Long, Long) = {
    unapplyDelta(ind, delta)
    evaluate(ind)
  }

  override def unapplyDelta(ind: Individual, delta: OrderedSet[Int]): Unit = {
    var i = delta.size
    while (i > 0) {
      i -= 1
      ind.flip(delta(i), weights, values)
    }
  }

  override def createStorage(problemSize: Int): Individual = new Individual(problemSize)
  override def initializeRandomly(individual: Individual, rng: ThreadLocalRandom): Unit = {
    var i = problemSize
    while (i > 0) {
      i -= 1
      if (rng.nextBoolean()) individual.flip(i, weights, values)
    }
  }
}

object RandomKnapsack {
  class Individual(problemSize: Int) {
    private[this] val choices = Array.fill(problemSize)(false)
    private[this] var sumWeights, sumValues = 0L

    def flip(bit: Int, weights: Array[Int], values: Array[Int]): Unit = {
      if (choices(bit)) {
        choices(bit) = false
        sumWeights -= weights(bit)
        sumValues -= values(bit)
      } else {
        choices(bit) = true
        sumWeights += weights(bit)
        sumValues += values(bit)
      }
    }

    def sumOfWeights: Long = sumWeights
    def sumOfValues: Long = sumValues
  }
}
