package ru.ifmo.onell.distribution

import java.util.Random

import ru.ifmo.onell.util.BinomialScanner

object BinomialDistribution {
  def apply(n: Long, p: Double): IntegerDistribution = {
    assert(n * p < Int.MaxValue / 2, s"The product of `n` and `p` is too large (${n * p}), sampling would be infeasible")

    new IntegerDistribution {
      override def sample(rng: Random): Int = withScanner(n, p, rng)
      override def minValue: Int = 0
      override val maxValue: Int = if (n > Int.MaxValue) Int.MaxValue else n.toInt
    }
  }

  def apply(n: Int, p: Double): IntegerDistribution = {
    new IntegerDistribution {
      override def sample(rng: Random): Int = withScanner(n, p, rng)
      override def minValue: Int = 0
      override val maxValue: Int = n
    }
  }

  private def withScanner(n: Long, p: Double, rng: Random): Int = {
    val sc = BinomialScanner(p)
    var idx = sc.offset(rng) - 1
    var result = 0
    while (idx < n) {
      result += 1
      idx += sc.offset(rng)
    }
    result
  }
}
