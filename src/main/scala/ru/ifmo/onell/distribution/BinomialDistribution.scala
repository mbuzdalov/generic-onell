package ru.ifmo.onell.distribution

import java.util.Random

import ru.ifmo.onell.util.BinomialScanner

object BinomialDistribution {
  def apply(n: Long, p: Double): IntegerDistribution = {
    assert(n * p < Int.MaxValue / 2, s"The product of `n` and `p` is too large (${n * p}), sampling would be infeasible")
    useBinomialScanner(if (n > Int.MaxValue) Int.MaxValue else n.toInt, p)
  }

  def apply(n: Int, p: Double): IntegerDistribution = useBinomialScanner(n, p)

  def useBinomialScanner(n: Int, p: Double): IntegerDistribution = {
    if (p == 0)
      0
    else if (p == 1)
      n
    else new IntegerDistribution {
      private[this] val scanner = BinomialScanner(p)
      override def sample(rng: Random): Int = {
        var idx = scanner.offset(rng) - 1
        var result = 0
        while (idx < n) {
          result += 1
          idx += scanner.offset(rng)
        }
        result
      }

      override def minValue: Int = 0
      override def maxValue: Int = n
    }
  }
}
