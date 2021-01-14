package ru.ifmo.onell.distribution

import java.util.Random

import scala.annotation.tailrec

import ru.ifmo.onell.util.BinomialScanner

object BinomialDistribution {
  def standard(n: Long, p: Double): IntegerDistribution = {
    assert(n * p < Int.MaxValue / 2, s"The product of `n` and `p` is too large (${n * p}), sampling would be infeasible")
    standard(if (n > Int.MaxValue) Int.MaxValue else n.toInt, p) // feasible until we have Long support from IntegerDistribution
  }

  def shift(n: Long, p: Double): IntegerDistribution = {
    assert(n * p < Int.MaxValue / 2, s"The product of `n` and `p` is too large (${n * p}), sampling would be infeasible")
    shift(if (n > Int.MaxValue) Int.MaxValue else n.toInt, p) // feasible until we have Long support from IntegerDistribution
  }

  def resampling(n: Long, p: Double): IntegerDistribution = {
    assert(n * p < Int.MaxValue / 2, s"The product of `n` and `p` is too large (${n * p}), sampling would be infeasible")
    resampling(if (n > Int.MaxValue) Int.MaxValue else n.toInt, p) // feasible until we have Long support from IntegerDistribution
  }

  def standard(n: Int, p: Double): IntegerDistribution = {
    if (n == 0)
      0
    else if (p == 0)
      0
    else if (p == 1)
      n
    else if (2.18e-8 * n * p < 2.6e-9 * n - 3.5e-8)
      new StandardWithScanner(n, p)
    else if (2.18e-8 * n * (1 - p) < 2.6e-9 * n - 3.5e-8)
      new StandardWithScannerInverted(n, p)
    else
      new StandardByDefinition(n, p)
  }

  def shift(n: Int, p: Double): IntegerDistribution =
    if (n == 0)
      throw new IllegalArgumentException("n is zero") // shall it be IntegerDistribution.empty?
    else if (p == 0)
      1
    else if (p == 1)
      n
    else if (2.18e-8 * n * p < 2.6e-9 * n - 3.5e-8)
      new ShiftWithScanner(n, p)
    else if (2.18e-8 * n * (1 - p) < 2.6e-9 * n - 3.5e-8)
      new ShiftWithScannerInverted(n, p)
    else
      new ShiftByDefinition(n, p)

  def resampling(n: Int, p: Double): IntegerDistribution =
    if (n == 0)
      throw new IllegalArgumentException("n is zero") // shall it be IntegerDistribution.empty?
    else if (p == 0)
      1
    else if (p == 1)
      n
    else if (2.18e-8 * n * p < 2.6e-9 * n - 3.5e-8)
      new ResamplingWithScanner(n, p)
    else if (2.18e-8 * n * (1 - p) < 2.6e-9 * n - 3.5e-8)
      new ResamplingWithScannerInverted(n, p)
    else
      new ResamplingByDefinition(n, p)

  private def sampleByDefinition(n: Int, p: Double, rng: Random): Int = {
    var i, rv = 0
    while (i < n) {
      if (rng.nextDouble() < p) rv += 1
      i += 1
    }
    rv
  }

  private[distribution] class StandardByDefinition(n: Int, p: Double) extends IntegerDistribution {
    override def minValue: Int = 0
    override def maxValue: Int = n
    override def sample(rng: Random): Int = sampleByDefinition(n, p, rng)
  }

  private[distribution] class ShiftByDefinition(n: Int, p: Double) extends IntegerDistribution {
    override def minValue: Int = 1
    override def maxValue: Int = n
    override def sample(rng: Random): Int = math.max(1, sampleByDefinition(n, p, rng))
  }

  private[distribution] class ResamplingByDefinition(n: Int, p: Double) extends IntegerDistribution {
    override def minValue: Int = 1
    override def maxValue: Int = n
    override def sample(rng: Random): Int = sampleImpl(rng)
    @tailrec private def sampleImpl(rng: Random): Int = {
      val value = sampleByDefinition(n, p, rng)
      if (value == 0) sampleImpl(rng) else value
    }
  }

  private def runScanner(n: Long, initValue: Long, scanner: BinomialScanner, rng: Random): Int = {
    var idx = initValue
    var result = 0
    while (idx < n) {
      result += 1
      idx += scanner.offset(rng)
    }
    result
  }

  private[distribution] class StandardWithScanner(n: Int, p: Double) extends IntegerDistribution {
    private[this] val scanner = BinomialScanner(p)
    override def minValue: Int = 0
    override def maxValue: Int = n
    override def sample(rng: Random): Int = runScanner(n, scanner.offset(rng) - 1, scanner, rng)
  }

  private[distribution] class ShiftWithScanner(n: Int, p: Double) extends IntegerDistribution {
    private[this] val scanner = BinomialScanner(p)
    override def minValue: Int = 1
    override def maxValue: Int = n
    override def sample(rng: Random): Int = math.max(1, runScanner(n, scanner.offset(rng) - 1, scanner, rng))
  }

  private[distribution] class ResamplingWithScanner(n: Int, p: Double) extends IntegerDistribution {
    private[this] val scanner = BinomialScanner(p)
    override def minValue: Int = 1
    override def maxValue: Int = n
    override def sample(rng: Random): Int = runScanner(n, (scanner.offset(rng) - 1) % n, scanner, rng)
  }

  private[distribution] class StandardWithScannerInverted(n: Int, p: Double) extends IntegerDistribution {
    private[this] val scanner = BinomialScanner(1 - p)
    override def minValue: Int = 1
    override def maxValue: Int = n
    override def sample(rng: Random): Int = n - runScanner(n, scanner.offset(rng) - 1, scanner, rng)
  }

  private[distribution] class ShiftWithScannerInverted(n: Int, p: Double) extends IntegerDistribution {
    private[this] val scanner = BinomialScanner(1 - p)
    override def minValue: Int = 1
    override def maxValue: Int = n
    override def sample(rng: Random): Int = math.max(1, n - runScanner(n, scanner.offset(rng) - 1, scanner, rng))
  }

  private[distribution] class ResamplingWithScannerInverted(n: Int, p: Double) extends IntegerDistribution {
    private[this] val scanner = BinomialScanner(1 - p)
    override def minValue: Int = 1
    override def maxValue: Int = n
    override def sample(rng: Random): Int = sampleImpl(rng)
    @tailrec private def sampleImpl(rng: Random): Int = {
      val value = n - runScanner(n, scanner.offset(rng) - 1, scanner, rng)
      if (value == 0) sampleImpl(rng) else value
    }
  }
}
