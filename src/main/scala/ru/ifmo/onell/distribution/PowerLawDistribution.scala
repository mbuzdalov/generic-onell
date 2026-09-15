package ru.ifmo.onell.distribution

import java.util.random.RandomGenerator
import scala.annotation.tailrec
import scala.collection.mutable

object PowerLawDistribution:
  def apply(n: Long, beta: Double): IntegerDistribution = PowerLawDistribution(math.min(n, Int.MaxValue).toInt, beta)
  def apply(n: Int, beta: Double): IntegerDistribution =
    if n < 1 then throw IllegalArgumentException("Power law distribution does not exist for a maximum value less than 1")
    else if n == 1 then 1
    else new IntegerDistribution:
      private val weights = collectWeightsUntilThreshold(beta, 1, n, 0, Array.newBuilder[Double])
      override def sample(rng: RandomGenerator): Int =
        val query = weights.last * rng.nextDouble()
        val index0 = java.util.Arrays.binarySearch(weights, query)
        if (index0 >= 0) index0 + 1 else -index0
      override def minValue: Int = 1
      override def maxValue: Int = weights.length

  @tailrec
  private def collectWeightsUntilThreshold(beta: Double, index: Int, size: Int, cumulative: Double,
                                           weights: mutable.ArrayBuilder[Double]): Array[Double] =
    val addend = cumulative + math.pow(index.toDouble, -beta)
    if index > size || addend == cumulative then weights.result() else
      weights += addend
      collectWeightsUntilThreshold(beta, index + 1, size, addend, weights)
