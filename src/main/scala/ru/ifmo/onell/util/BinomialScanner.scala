package ru.ifmo.onell.util

import java.util.random.RandomGenerator

abstract class BinomialScanner:
  def offset(rng: RandomGenerator): Long

object BinomialScanner:
  private object ProbabilityOneScanner extends BinomialScanner:
    override def offset(rng: RandomGenerator): Long = 1

  private object ProbabilityZeroScanner extends BinomialScanner:
    override def offset(rng: RandomGenerator): Long = Long.MaxValue

  private class NormalScanner(log1p: Double) extends BinomialScanner:
    override def offset(rng: RandomGenerator): Long = 1 + (math.log(rng.nextDouble()) / log1p).toLong

  def apply(probability: Double): BinomialScanner =
    if probability >= 1 then ProbabilityOneScanner
    else if probability <= 0 then ProbabilityZeroScanner
    else NormalScanner(math.log1p(-probability))

  def fromLogOneMinusP(log1p: Double): BinomialScanner = NormalScanner(log1p)
