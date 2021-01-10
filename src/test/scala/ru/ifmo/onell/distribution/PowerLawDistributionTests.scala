package ru.ifmo.onell.distribution

import java.util.concurrent.ThreadLocalRandom

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PowerLawDistributionTests extends AnyFlatSpec with Matchers {
  def testMaxInt(beta: Double): Unit = PowerLawDistribution(Int.MaxValue, beta).sample(ThreadLocalRandom.current())

  "Power law distribution" should "fit space with beta=2.3 for large n" in testMaxInt(2.3)
  it should "fit space with beta=2.5 for large n" in testMaxInt(2.5)
  it should "fit space with beta=2.7 for large n" in testMaxInt(2.7)
  it should "fit space with beta=2.9 for large n" in testMaxInt(2.9)
  it should "even take large longs normally" in PowerLawDistribution(1L << 50, 2.5).sample(ThreadLocalRandom.current())
}
