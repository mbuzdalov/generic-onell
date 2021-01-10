package ru.ifmo.onell.algorithm

import scala.Ordering.Double.IeeeOrdering

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import ru.ifmo.onell.Optimizer
import ru.ifmo.onell.problem.{LinearRandomDoubleWeights, LinearRandomIntegerWeights, OneMax, OneMaxPerm, RandomPlanted3SAT}

class RLSTests extends AnyFlatSpec with Matchers {
  private def RLS = OnePlusOneEA.RLS

  "RLS" should "perform as expected on OneMax" in {
    val size = 200
    val om = new OneMax(size)
    val runs = IndexedSeq.fill(100)(RLS.optimize(om))
    val expected = size * (1 to size / 2).map(1.0 / _).sum
    val found = runs.sum.toDouble / runs.size
    found should (be <= expected * 1.1)
  }

  it should "perform as expected on OneMaxPerm" in {
    val size = 200
    val om = new OneMaxPerm(size)
    val runs = IndexedSeq.fill(20)(RLS.optimize(om))
    val expected = size / 2.0 * size * (1 to size / 2).map(1.0 / _).sum
    val found = runs.sum.toDouble / runs.size
    found should (be <= expected * 1.21)
  }

  it should "perform as expected on OneMaxPerm even if called through interface" in {
    val size = 200
    val om = new OneMaxPerm(size)
    val rls: Optimizer = RLS
    val runs = IndexedSeq.fill(20)(rls.optimize(om))
    val expected = size / 2.0 * size * (1 to size / 2).map(1.0 / _).sum
    val found = runs.sum.toDouble / runs.size
    found should (be <= expected * 1.21)
  }

  it should "perform as expected on LinearRandomDoubleWeights" in {
    val size = 200
    val w = new LinearRandomDoubleWeights(size, 5.0, 127324)
    val runs = IndexedSeq.fill(100)(RLS.optimize(w))
    val expected = size * (1 to size / 2).map(1.0 / _).sum
    val found = runs.sum.toDouble / runs.size
    found should (be <= expected * 1.1)
  }

  it should "perform as expected on LinearRandomIntegerWeights" in {
    val size = 200
    val w = new LinearRandomIntegerWeights(size, 5, 824354325)
    val runs = IndexedSeq.fill(100)(RLS.optimize(w))
    val expected = size * (1 to size / 2).map(1.0 / _).sum
    val found = runs.sum.toDouble / runs.size
    found should (be <= expected * 1.1)
  }

  it should "perform as expected on RandomPlanted3SAT" in {
    val size = 200
    val sat = new RandomPlanted3SAT(size, size * 20, RandomPlanted3SAT.EasyGenerator, 82365436465L)
    val runs = IndexedSeq.fill(10)(RLS.optimize(sat))
    val expected = size * (1 to size / 2).map(1.0 / _).sum
    val found = runs.sum.toDouble / runs.size
    found should (be <= expected * 1.5)
  }

  it should "log improvements correctly" in {
    val size = 200
    val om = new OneMax(size)
    val logger = new ValidationLogger
    val calls = RLS.optimize(om, logger)
    logger.fitness shouldBe 200
    logger.evaluations shouldBe calls
  }
}
