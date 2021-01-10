package ru.ifmo.onell.distribution

import java.util.Random

import ru.ifmo.onell.util.BinomialScanner

object BinomialDistribution {
  def apply(n: Long, p: Double): IntegerDistribution = {
    assert(n * p < Int.MaxValue / 2, s"The product of `n` and `p` is too large (${n * p}), sampling would be infeasible")

    new IntegerDistribution {
      private[this] val scanner = BinomialScanner(p)
      override def sample(rng: Random): Int = withScanner(n, scanner, rng)
      override def minValue: Int = 0
      override val maxValue: Int = if (n > Int.MaxValue) Int.MaxValue else n.toInt
    }
  }

  def apply(n: Int, p: Double): IntegerDistribution = {
    new IntegerDistribution {
      private[this] val scanner = BinomialScanner(p)
      override def sample(rng: Random): Int = withScanner(n, scanner, rng)
      override def minValue: Int = 0
      override val maxValue: Int = n
    }
  }

  private def withScanner(n: Long, scanner: BinomialScanner, rng: Random): Int = {
    var idx = scanner.offset(rng) - 1
    var result = 0
    while (idx < n) {
      result += 1
      idx += scanner.offset(rng)
    }
    result
  }
}
