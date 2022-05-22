package ru.ifmo.onell.algorithm

import scala.Ordering.Double.IeeeOrdering

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import ru.ifmo.onell.{HasIndividualOperations, Optimizer}
import ru.ifmo.onell.problem.mst.TreeOnlyMST
import ru.ifmo.onell.problem.mst.util.NaiveDynamicGraph
import ru.ifmo.onell.problem.{OneMax, OneMaxPerm}

class OnePlusOneEATests extends AnyFlatSpec with Matchers {
  "(1+1) EA" should "perform as expected on OneMax" in {
    val size = 200
    val om = new OneMax(size)
    val runs = IndexedSeq.fill(100)(OnePlusOneEA.Resampling.optimize(om))
    val expected = size * (1 to size / 2).map(1.0 / _).sum * (math.E - 1)
    val found = runs.sum.toDouble / runs.size
    found should (be <= expected)
  }

  it should "perform as expected on OneMaxPerm" in {
    val size = 200
    val om = new OneMaxPerm(size)
    val runs = IndexedSeq.fill(20)(OnePlusOneEA.Resampling.optimize(om))
    val expected = size / 2.0 * size * (1 to size / 2).map(1.0 / _).sum * (math.E - 1)
    runs.count(_ < expected) should (be >= 7)
  }

  it should "perform as expected on OneMaxPerm even if called through interface" in {
    val size = 200
    val om = new OneMaxPerm(size)
    val opo: Optimizer = OnePlusOneEA.Resampling
    val runs = IndexedSeq.fill(20)(opo.optimize(om))
    val expected = size / 2.0 * size * (1 to size / 2).map(1.0 / _).sum * (math.E - 1)
    runs.count(_ < expected) should (be >= 7)
  }

  it should "perform as expected on the tree-only MST problem" in {
    val rng = new java.util.Random(872454326413212L)
    val mst = TreeOnlyMST.randomGraph(256, 512, 1, 2, new java.util.Random(rng.nextLong()), NaiveDynamicGraph)
    implicit val individualOps: HasIndividualOperations[TreeOnlyMST.Individual] = mst
    val runtimes = (0 until 5).map(_ => OnePlusOneEA.Resampling.optimize(mst))
    runtimes.count(v => v >= 70000 && v <= 150000) should (be >= 2)
  }

  it should "log improvements correctly" in {
    val size = 200
    val om = new OneMax(size)
    val logger = new ValidationLogger
    val calls = OnePlusOneEA.Resampling.optimize(om, logger)
    logger.fitness shouldBe 200
    logger.evaluations shouldBe calls
  }
}
