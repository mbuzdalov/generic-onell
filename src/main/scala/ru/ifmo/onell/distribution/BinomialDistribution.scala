package ru.ifmo.onell.distribution

import java.util.Random

import ru.ifmo.onell.util.BinomialScanner

object BinomialDistribution {
  def apply(n: Long, p: Double): IntegerDistribution = {
    assert(n * p < Int.MaxValue / 2, s"The product of `n` and `p` is too large (${n * p}), sampling would be infeasible")
    BinomialDistribution(if (n > Int.MaxValue) Int.MaxValue else n.toInt, p)
  }

  def apply(n: Int, p: Double): IntegerDistribution =
    if (p == 0)
      0
    else if (p > 0.5)
      BinomialDistribution(n, 1 - p).negate().plus(n)
    else if (2.18e-8 * n * p < 2.6e-9 * n - 3.5e-8) // this is an empirical relation based on linear regressions
      new WithScanner(n, p)
    else
      new ByDefinition(n, p)

  private[distribution] class ByDefinition(n: Int, p: Double) extends IntegerDistribution {
    override def sample(rng: Random): Int = {
      var i, rv = 0
      while (i < n) {
        if (rng.nextDouble() < p) rv += 1
        i += 1
      }
      rv
    }
    override def minValue: Int = 0
    override def maxValue: Int = n
  }

  private[distribution] class WithScanner(n: Int, p: Double) extends IntegerDistribution {
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
