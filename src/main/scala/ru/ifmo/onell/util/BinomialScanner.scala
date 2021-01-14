package ru.ifmo.onell.util

import java.util.Random

abstract class BinomialScanner {
  def offset(rng: Random): Long
}

object BinomialScanner {
  private[this] object ProbabilityOneScanner extends BinomialScanner {
    override def offset(rng: Random): Long = 1
  }

  private[this] object ProbabilityZeroScanner extends BinomialScanner {
    override def offset(rng: Random): Long = Long.MaxValue
  }

  private[this] class NormalScanner(log1p: Double) extends BinomialScanner {
    override def offset(rng: Random): Long = 1 + (math.log(rng.nextDouble()) / log1p).toLong
  }

  def apply(probability: Double): BinomialScanner =
    if (probability >= 1)
      ProbabilityOneScanner
    else if (probability <= 0)
      ProbabilityZeroScanner
    else
      new NormalScanner(math.log1p(-probability))

  def fromLogOneMinusP(log1p: Double): BinomialScanner = new NormalScanner(log1p)
}
