package ru.ifmo.onell.algorithm

import scala.Ordering.Double.IeeeOrdering

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA._
import ru.ifmo.onell.problem.{OneMax, OneMaxPerm, RandomPlanted3SAT}

class OnePlusLambdaLambdaGATests extends AnyFlatSpec with Matchers {
  "(1+LL) GA" should "perform as expected on OneMax" in {
    val size = 200
    val om = new OneMax(size)
    val ll = new OnePlusLambdaLambdaGA(defaultOneFifthLambda, 'R', "RL", 'C', 'U')
    val runs = IndexedSeq.fill(100)(ll.optimize(om))
    val found = runs.sum.toDouble / runs.size
    found should (be <= 1300.0)
    found should (be >= 1100.0)
  }

  it should "perform as expected on OneMax using a power-law distribution" in {
    val size = 200
    val om = new OneMax(size)
    val ll = new OnePlusLambdaLambdaGA(powerLawLambda(2.5), 'R', "RL", 'C', 'U')
    val runs = IndexedSeq.fill(100)(ll.optimize(om))
    val found = runs.sum.toDouble / runs.size
    found should (be <= 1700.0)
    found should (be >= 1400.0)
  }


  it should "perform as expected on OneMax without being practice-aware" in {
    val size = 200
    val om = new OneMax(size)
    val ll = new OnePlusLambdaLambdaGA(defaultOneFifthLambda, 'S', "SL", 'I', 'U')
    val runs = IndexedSeq.fill(100)(ll.optimize(om))
    val found = runs.sum.toDouble / runs.size
    found should (be <= 2120.0)
    found should (be >= 1800.0)
  }

  it should "perform as expected on OneMaxPerm" in {
    val size = 200
    val om = new OneMaxPerm(size)
    val ll = new OnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'R', "RL", 'C', 'U')
    val runs = IndexedSeq.fill(10)(ll.optimize(om))
    val found = runs.sum.toDouble / runs.size
    found should (be <= 1.2e5)
  }

  it should "perform as expected on RandomPlanted3SAT with log capping" in {
    val size = 200
    val om = new RandomPlanted3SAT(size, size * 20, RandomPlanted3SAT.EasyGenerator, 3454353454545L)
    val ll = new OnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'R', "RL", 'C', 'U')
    val runs = IndexedSeq.fill(100)(ll.optimize(om))
    val found = runs.sum.toDouble / runs.size
    found should (be <= 1900.0)
    found should (be >= 1300.0)
  }

  it should "log improvements correctly" in {
    val size = 200
    val om = new OneMax(size)
    val logger = new ValidationLogger
    val ll = new OnePlusLambdaLambdaGA(defaultOneFifthLambda, 'R', "RL", 'C', 'U')
    val calls = ll.optimize(om, logger)
    logger.fitness shouldBe 200
    logger.evaluations shouldBe calls
  }
}
