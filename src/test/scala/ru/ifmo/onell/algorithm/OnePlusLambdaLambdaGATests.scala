package ru.ifmo.onell.algorithm

import scala.Ordering.Double.IeeeOrdering

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import ru.ifmo.onell.HasIndividualOperations
import ru.ifmo.onell.algorithm.oll.CompatibilityLayer.*
import ru.ifmo.onell.problem.mst.TreeOnlyMST
import ru.ifmo.onell.problem.mst.util.NaiveDynamicGraph
import ru.ifmo.onell.problem.{OneMax, OneMaxPerm, RandomPlanted3SAT}
import java.util.Random

class OnePlusLambdaLambdaGATests extends AnyFlatSpec with Matchers:
  "(1+LL) GA" should "perform as expected on OneMax" in:
    val size = 200
    val om = OneMax(size)
    val ll = createOnePlusLambdaLambdaGA(defaultOneFifthLambda, 'R', "RL", 'C', 'U')
    val runs = IndexedSeq.fill(100)(ll.optimize(om))
    val found = runs.sum.toDouble / runs.size
    found should (be <= 1300.0)
    found should (be >= 1100.0)

  it should "perform as expected on OneMax using a power-law distribution" in:
    val size = 200
    val om = OneMax(size)
    val ll = createOnePlusLambdaLambdaGA(powerLawLambda(2.5), 'R', "RL", 'C', 'U')
    val runs = IndexedSeq.fill(100)(ll.optimize(om))
    val found = runs.sum.toDouble / runs.size
    found should (be <= 1700.0)
    found should (be >= 1400.0)

  it should "perform as expected on OneMax without being practice-aware" in:
    val size = 200
    val om = OneMax(size)
    val ll = createOnePlusLambdaLambdaGA(defaultOneFifthLambda, 'S', "SL", 'I', 'U')
    val runs = IndexedSeq.fill(200)(ll.optimize(om))
    val found = runs.sum.toDouble / runs.size
    found should (be <= 2120.0)
    found should (be >= 1800.0)

  it should "perform as expected on OneMaxPerm" in:
    val size = 200
    val om = OneMaxPerm(size)
    val ll = createOnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'R', "RL", 'C', 'U')
    val runs = IndexedSeq.fill(10)(ll.optimize(om))
    val found = runs.sum.toDouble / runs.size
    found should (be <= 1.2e5)

  it should "perform as expected on RandomPlanted3SAT with log capping" in:
    val size = 200
    val om = RandomPlanted3SAT(size, size * 20, RandomPlanted3SAT.EasyGenerator, 3454353454545L)
    val ll = createOnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'R', "RL", 'C', 'U')
    val runs = IndexedSeq.fill(100)(ll.optimize(om))
    val found = runs.sum.toDouble / runs.size
    found should (be <= 1900.0)
    found should (be >= 1300.0)

  it should "perform as expected on the tree-only MST problem (lambda <= 2 ln n)" in:
    val rng = Random(872454326413212L)
    val mst = TreeOnlyMST.randomGraph(256, 512, 1, 2, Random(rng.nextLong()), NaiveDynamicGraph)
    val optimizer = createOnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'R', "RL", 'C', 'D')
    val runtimes = (0 until 5).map(_ => optimizer.optimize(mst)(using indOps = mst))
    runtimes.count(v => v >= 100000 && v <= 250000) should (be >= 2)

  it should "perform as expected on the tree-only MST problem (lambda = 10)" in:
    val rng = Random(872454326413212L)
    val mst = TreeOnlyMST.randomGraph(256, 512, 1, 2, Random(rng.nextLong()), NaiveDynamicGraph)
    val optimizer = createOnePlusLambdaLambdaGA(fixedLambda(10), 'R', "RL", 'C', 'D')
    val runtimes = (0 until 5).map(_ => optimizer.optimize(mst)(using indOps = mst))
    runtimes.count(v => v >= 80000 && v <= 180000) should (be >= 2)

  it should "log improvements correctly" in:
    val size = 200
    val om = OneMax(size)
    val logger = ValidationLogger()
    val ll = createOnePlusLambdaLambdaGA(defaultOneFifthLambda, 'R', "RL", 'C', 'U')
    val calls = ll.optimize(om, logger)
    logger.fitness shouldBe 200
    logger.evaluations shouldBe calls
