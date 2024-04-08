package ru.ifmo.onell.main

import java.io.PrintWriter
import java.util.concurrent.{Callable, Executors, TimeUnit}
import java.util.{Locale, Random}

import scala.jdk.CollectionConverters._
import scala.util.Using
import scala.Ordering.Double.TotalOrdering

import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA
import ru.ifmo.onell.algorithm.oll.CompatibilityLayer._
import ru.ifmo.onell.main.util.AlgorithmCodeNames
import ru.ifmo.onell.problem.HammingDistance._
import ru.ifmo.onell.problem.{LinearRandomIntegerWeights, OneMaxPerm, RandomPlanted3SAT}
import ru.ifmo.onell.util.Specialization
import ru.ifmo.onell.{Fitness, IterationLogger, Main}

object LambdaColorMap extends Main.Module {
  override def name: String = "lambda-color-map"

  override def shortDescription: String = "Runs experiments on the expected improvements depending on λ"

  override def longDescription: Seq[String] = Seq(
    "This module supports the following commands:",
    "  bits:li <options>: runs the experiments for random linear functions",
    "                     with integer weights on bit strings.",
    "        Options are:",
    "             --n            <int>: the problem size",
    "             --runs         <int>: the number of runs for each λ",
    "             --weight       <int>: the maximum allowed weight",
    "             --lambda-power <double>: the multiplicative step for λ to use",
    "             --tuning       <tuning>: the tuning(s) to use",
    "             --out-prefix   <string>: the filename prefix to use",
    "  bits:sat <options>: runs the experiments for random easy MAX-SAT instances.",
    "        Options are:",
    "             --n            <int>: the problem size",
    "             --runs         <int>: the number of runs for each λ",
    "             --lambda-power <double>: the multiplicative step for λ to use",
    "             --tuning       <tuning>: the tuning(s) to use",
    "             --out-prefix   <string>: the filename prefix to use",
    "  perm:om <options>: runs the experiments for OneMax on permutations.",
    "        Options are:",
    "             --n            <int>: the problem size",
    "             --runs         <int>: the number of runs for each λ",
    "             --max-lambda   <double>: the maximum λ to use",
    "             --lambda-power <double>: the multiplicative step for λ to use",
    "             --tuning       <tuning>: the tuning(s) to use",
    "             --out-prefix   <string>: the filename prefix to use",
    ""
  ) ++ AlgorithmCodeNames.parserDescriptionForOnePlusLambdaLambdaGenerators("--tuning")

  override def moduleMain(args: Array[String]): Unit = args(0) match {
    case "bits:li" =>
      val weight = args.getOption("--weight").toInt
      collect3DPlots(
        problemInstanceGen = (n, seed) => new LinearRandomIntegerWeights(n, weight, seed),
        n = args.getOption("--n").toInt,
        runs = args.getOption("--runs").toInt,
        lambdaPower = args.getOption("--lambda-power").toDouble,
        tuningMask = args.getOption("--tuning"),
        filePrefix = args.getOption("--out-prefix"))
    case "bits:sat" =>
      collect3DPlots(
        problemInstanceGen = (n, seed) => new RandomPlanted3SAT(n, (4 * n * math.log(n)).toInt, RandomPlanted3SAT.EasyGenerator, seed),
        n = args.getOption("--n").toInt,
        runs = args.getOption("--runs").toInt,
        lambdaPower = args.getOption("--lambda-power").toDouble,
        tuningMask = args.getOption("--tuning"),
        filePrefix = args.getOption("--out-prefix"))
    case "perm:om" =>
      collect3DPlotsPerm(
        n = args.getOption("--n").toInt,
        runs = args.getOption("--runs").toInt,
        lambdaPower = args.getOption("--lambda-power").toDouble,
        maxLambda = args.getOption("--max-lambda").toDouble,
        tuningMask = args.getOption("--tuning"),
        filePrefix = args.getOption("--out-prefix"))
  }

  private class HammingImprovementStatistics(val size: Int) {
    private[this] val hammingCounts, hammingIncrements = new Array[Long](size + 1)
    private[this] val countIncrements = new Array[Long](6)

    def consume(distance: Int, evaluations: Long, increment: Long): Unit = synchronized {
      hammingCounts(distance) += evaluations
      hammingIncrements(distance) += increment
      countIncrements(math.min(5, increment).toInt) += 1
    }

    def incrementStats: String = s"1: ${countIncrements(1)}, 2: ${countIncrements(2)}, 3: ${countIncrements(3)}, 4: ${countIncrements(4)}, 5+: ${countIncrements(5)}"

    def extract(target: Array[Double]): Unit =
      for (i <- 1 until target.length)
        target(i) = hammingIncrements(i).toDouble / hammingCounts(i)
  }

  private class HammingImprovementCollector[@specialized(Specialization.fitnessSpecialization) F]
    (stats: HammingImprovementStatistics)(implicit fitness2long: F => Long)
    extends IterationLogger[FAHD[F]]
  {
    private[this] var lastEvaluations = 0L
    private[this] var lastFitness: F = _
    private[this] var lastDistance = -1

    override def logIteration(evaluations: Long, fitness: FAHD[F]): Unit = {
      if (evaluations == 1) { // start
        lastEvaluations = 1
        lastDistance = fitness.distance
        lastFitness = fitness.fitness
      } else {
        if (fitness.fitness > lastFitness) {
          stats.consume(lastDistance, evaluations - lastEvaluations, lastDistance - fitness.distance)
          lastEvaluations = evaluations
          lastDistance = fitness.distance
          lastFitness = fitness.fitness
        }
      }
    }
  }

  private class PermImprovementCollector(stats: HammingImprovementStatistics)
    extends IterationLogger[Int]
  {
    private[this] var lastEvaluations = 0L
    private[this] var lastDistance = -1

    override def logIteration(evaluations: Long, fitness: Int): Unit = {
      val distance = stats.size - fitness
      if (evaluations == 1) { // start
        lastEvaluations = 1
        lastDistance = distance
      } else {
        if (distance < lastDistance) {
          stats.consume(lastDistance, evaluations - lastEvaluations, lastDistance - distance)
          lastEvaluations = evaluations
          lastDistance = distance
        }
      }
    }
  }

  private def to3dPlotNumber(v: Double): String = String.format(Locale.US, "%.2e", v)

  //noinspection SameParameterValue: IDEA wrongly reports `file` to have the same parameter value for interpolated arg
  private def collect3DPlots[@specialized(Specialization.fitnessSpecialization) F]
                            (optimizerFromLambda: (Long => LambdaTuning) => OnePlusLambdaLambdaGA,
                             problemInstanceGen: (Int, Long) => Fitness[Array[Boolean], F, Int],
                             n: Int, runs: Int, lambdaPower: Double, file: String)
                            (implicit fitness2long: F => Long): Unit = {
    val executor = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
    Using.resources(
      new PrintWriter(file + ".txt.cmp"),
      new PrintWriter(file + ".raw.cmp"),
    ) { (out, raw) =>
      val rng = new Random(9234352524211L)
      val maxLambda = (math.log(n) / 1.25 / math.log(lambdaPower)).toInt
      val arrays = Array.ofDim[Double](maxLambda, n / 2 + 1)

      def lambdaGenFun(lg: Int): Double = math.pow(lambdaPower, lg)

      for (lambdaGen <- arrays.indices; lambda = lambdaGenFun(lambdaGen)) {
        val oll = optimizerFromLambda(fixedLambda(lambda))
        val logger = new HammingImprovementStatistics(n)

        def newCallable(): Callable[Unit] = () => oll.optimize(
          problemInstanceGen(n, rng.nextLong()).withHammingDistanceTracking,
          new HammingImprovementCollector[F](logger))

        executor.invokeAll((0 until runs).map(_ => newCallable()).asJava).forEach(_.get())
        logger.extract(arrays(lambdaGen))
        println(s"[$file]: lambda $lambda done")
      }

      raw.println(s"lambda power $lambdaPower")
      raw.println("distance offset 1")
      for (a <- arrays)
        raw.println(a.view.slice(1, n / 2 + 1).map(to3dPlotNumber).mkString(" "))

      for (dist <- 1 to n / 2) {
        val maxAcross = arrays.view.map(_(dist)).max
        for (y <- arrays.indices)
          arrays(y)(dist) /= maxAcross
      }

      out.println(s"lambda power $lambdaPower")
      out.println("distance offset 1")
      for (a <- arrays)
        out.println(a.view.slice(1, n / 2 + 1).map(to3dPlotNumber).mkString(" "))
    }
    executor.shutdown()
    executor.awaitTermination(365, TimeUnit.DAYS)
  }

  private def collect3DPlots[@specialized(Specialization.fitnessSpecialization) F]
                            (problemInstanceGen: (Int, Long) => Fitness[Array[Boolean], F, Int],
                             n: Int, runs: Int, lambdaPower: Double,
                             tuningMask: String, filePrefix: String)
                            (implicit fitness2long: F => Long): Unit =
    for ((algFun, code) <- AlgorithmCodeNames.parseOnePlusLambdaLambdaGenerators(tuningMask))
      collect3DPlots(algFun, problemInstanceGen, n, runs, lambdaPower, s"$filePrefix-$code")

  //noinspection SameParameterValue: IDEA wrongly reports `file` to have the same parameter value for interpolated arg
  private def collect3DPlotsPerm(optimizerFromLambda: (Long => LambdaTuning) => OnePlusLambdaLambdaGA,
                                  n: Int, runs: Int, lambdaPower: Double, maxLambda: Double, file: String): Unit = {
    val executor = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
    Using.resources(
      new PrintWriter(file + ".txt.cmp"),
      new PrintWriter(file + ".raw.cmp"),
    ) { (out, raw) =>
      val maxLambdaIndex = 1 + (math.log(maxLambda) / math.log(lambdaPower)).toInt
      val arrays = Array.ofDim[Double](maxLambdaIndex, n + 1)

      def lambdaGenFun(lg: Int): Double = math.pow(lambdaPower, lg)

      for (lambdaGen <- arrays.indices; lambda = lambdaGenFun(lambdaGen)) {
        val fun = new OneMaxPerm(n)
        val oll = optimizerFromLambda(fixedLambda(lambda))
        val logger = new HammingImprovementStatistics(n)

        def newCallable(): Callable[Unit] = () => oll.optimize(fun, new PermImprovementCollector(logger))

        executor.invokeAll((0 until runs).map(_ => newCallable()).asJava).forEach(_.get())
        logger.extract(arrays(lambdaGen))
        println(s"[$file]: lambda $lambda done, improvement stats: ${logger.incrementStats}")
      }

      raw.println(s"lambda power $lambdaPower")
      raw.println(s"distance offset 2")
      for (a <- arrays)
        raw.println(a.view.slice(2, n + 1).map(to3dPlotNumber).mkString(" "))

      for (dist <- 2 to n) {
        val maxAcross = arrays.view.map(_ (dist)).max
        for (y <- arrays.indices)
          arrays(y)(dist) /= maxAcross
      }

      out.println(s"lambda power $lambdaPower")
      out.println(s"distance offset 2")
      for (a <- arrays)
        out.println(a.view.slice(2, n + 1).map(to3dPlotNumber).mkString(" "))
    }
    executor.shutdown()
    executor.awaitTermination(365, TimeUnit.DAYS)
  }

  private def collect3DPlotsPerm(n: Int, runs: Int, lambdaPower: Double, maxLambda: Double,
                                 tuningMask: String, filePrefix: String): Unit =
    for ((algFun, code) <- AlgorithmCodeNames.parseOnePlusLambdaLambdaGenerators(tuningMask))
      collect3DPlotsPerm(algFun, n, runs, lambdaPower, maxLambda, s"$filePrefix-$code")

  private implicit class Options(val args: Array[String]) extends AnyVal {
    def getOption(option: String): String = {
      val index = args.indexOf(option)
      if (index < 0) throw new IllegalArgumentException(s"No option '$option' is given")
      if (index + 1 == args.length) throw new IllegalArgumentException(s"Option '$option' should have an argument")
      args(index + 1)
    }
  }
}
