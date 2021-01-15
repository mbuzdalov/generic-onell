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
    checkProbability(p)
    if (n == 0)
      0
    else if (p == 0)
      0
    else if (p == 1)
      n
    else {
      val rhs = 2.6e-9 * n - 3.5e-8
      val lhs = 2.18e-8 * n
      if (lhs * p < rhs)
        new StandardWithScanner(n, p)
      else if (lhs * (1 - p) < rhs)
        new StandardWithScannerInverted(n, p)
      else
        new StandardByDefinition(n, p)
    }
  }

  def shift(n: Int, p: Double): IntegerDistribution = {
    checkProbability(p)
    if (n == 0)
      IntegerDistribution.empty
    else if (n == 1 || p == 0)
      1
    else if (p == 1)
      n
    else {
      val rhs = 2.6e-9 * n - 3.5e-8
      val lhs = 2.18e-8 * n
      if (lhs * p < rhs)
        new ShiftWithScanner(n, p)
      else if (lhs * (1 - p) + 3.5e-8 < rhs)
        new ShiftWithScannerInverted(n, p)
      else
        new ShiftByDefinition(n, p)
    }
  }

  def resampling(n: Int, p: Double): IntegerDistribution = {
    checkProbability(p)
    if (n == 0)
      IntegerDistribution.empty
    else if (n == 1 || p == 0)
      1
    else if (p == 1)
      n
    else if (n == 2)
      new ResamplingByDefinition2(p)
    else if (n == 3)
      new ResamplingByDefinition3(p)
    else {
      val log1p = math.log1p(-p)
      val rhs = -2.6e-9 * n / math.expm1(n * log1p) - 6e-8
      val lhs = 2.18e-8 * n
      if (lhs * p < rhs)
        new ResamplingWithScanner(n, log1p, n)
      else if (lhs * (1 - p) < rhs)
        new ResamplingWithScannerInverted(n, p)
      else
        new ResamplingByDefinition(n, p)
    }
  }

  private def checkProbability(p: Double): Unit = {
    assert(!p.isNaN, "The probability cannot be NaN")
    assert(p >= 0, "The probability cannot be negative")
    assert(p <= 1, "The probability cannot be greater than 1")
  }

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

  private[distribution] class ResamplingByDefinitionWithSpecialPrefix(n: Int, p: Double) extends IntegerDistribution {
    private[this] val minusP = -p
    private[this] val log1p = math.log1p(minusP)

    override def minValue: Int = 1
    override def maxValue: Int = n
    override def sample(rng: Random): Int = runPrefix(rng, n)

    private def queryOne(rng: Random, idx: Int): Boolean = {
      val sample = rng.nextDouble()
      sample < p || sample < minusP / math.expm1(idx * log1p)
    }

    @tailrec
    private def runPrefix(rng: Random, idx: Int): Int =
      if (idx == 1)
        1
      else if (queryOne(rng, idx))
        1 + sampleByDefinition(idx - 1, p, rng)
      else
        runPrefix(rng, idx - 1)
  }

  private[distribution] class ResamplingByDefinition2(p: Double) extends IntegerDistribution {
    private[this] val p2 = p / (2 - p)
    override def minValue: Int = 1
    override def maxValue: Int = 2
    override def sample(rng: Random): Int = if (rng.nextDouble() < p2) 2 else 1
  }

  private[distribution] class ResamplingByDefinition3(p: Double) extends IntegerDistribution {
    private[this] val pp = p * p
    private[this] val p3 = p * 3
    private[this] val pOf3 = pp / (3 - p3 + pp)
    private[this] val pOf23 = (p3 - 2 * pp) / (3 - p3 + pp)
    override def minValue: Int = 1
    override def maxValue: Int = 3
    override def sample(rng: Random): Int = {
      val sample = rng.nextDouble()
      if (sample < pOf3) 3
      else if (sample < pOf23) 2
      else 1
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

  // Here, for performance reasons, we directly use log1p that has been computed earlier.
  // However, this is misleading even if calling this class from tests, so we change the signature in such a tricky way.
  private[distribution] class ResamplingWithScanner private[BinomialDistribution] (n: Int, log1p: Double, anotherN: Int)
    extends IntegerDistribution
  {
    // This is the constructor for the outside callers (where "outside" means tests)
    // that matches the signatures of all other constructors.
    def this(n: Int, p: Double) = this(n, math.log1p(-p), n)

    assert(n == anotherN, "The caller of this class does not know what they do")
    private[this] val scanner = BinomialScanner.fromLogOneMinusP(log1p)
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
