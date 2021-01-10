package ru.ifmo.onell.distribution

import java.util.Random

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BinomialDistributionTests extends AnyFlatSpec with Matchers {
  private type DistGen = (Int, Double) => IntegerDistribution

  private def test(n: Int, p: Double, runs: Int, distribution: DistGen): Unit = {
    var nZeros, nOnes = 0
    val dist = distribution(n, p)
    val rng = new Random(78438753652362575L)
    var count = 0
    while (count < runs) {
      val sample = dist.sample(rng)
      sample match {
        case 0 => nZeros += 1
        case 1 => nOnes += 1
        case _ =>
      }
      count += 1
    }

    val expectedZeros = math.pow(1 - p, n) * runs
    val expectedOnes = math.pow(1 - p, n - 1) * p * n * runs
    val tolerance = math.sqrt(n) * math.log(n) / n

    val zeroLo = math.floor(expectedZeros * (1 - tolerance)).toInt
    val zeroHi = math.ceil(expectedZeros * (1 + tolerance)).toInt
    nZeros should (be >= zeroLo and be <= zeroHi)

    val oneLo = math.floor(expectedOnes * (1 - tolerance)).toInt
    val oneHi = math.ceil(expectedOnes * (1 + tolerance)).toInt
    nOnes should (be >= oneLo and be <= oneHi)
  }

  for ((d, name) <- Seq[(DistGen, String)](
    ((n, p) => BinomialDistribution(n, p), "The default implementation"),
    (BinomialDistribution.useBinomialScanner, "Implementation using binomial scanner"),
    (BinomialDistribution.useDefinition, "Implementation by definition"),
  )) {
    name should "produce expected results for n=100, p=0.01" in test(100, 0.01, 5000, d)
    it should "produce expected results for n=100, p=0.03" in test(100, 0.03, 5000, d)
    it should "produce expected results for n=100, p=0.1" in test(100, 0.1, 10000, d)
    it should "produce expected results for n=1000, p=0.001" in test(1000, 0.001, 30000, d)
    it should "produce expected results for n=1000, p=0.02" in test(1000, 0.02, 30000, d)
    it should "produce expected results for n=1000, p=0.1" in test(1000, 0.1, 30000, d)
  }
}
