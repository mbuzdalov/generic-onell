package ru.ifmo.onell.main

import java.awt.image.BufferedImage
import java.io.{File, PrintWriter}
import java.util.concurrent.{Callable, Executors, TimeUnit}
import java.util.{Locale, Random}
import javax.imageio.ImageIO
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.Ordering.Double.TotalOrdering
import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA
import ru.ifmo.onell.algorithm.oll.CompatibilityLayer.*
import ru.ifmo.onell.distribution.BinomialDistribution
import ru.ifmo.onell.main.util.AlgorithmCodeNames
import ru.ifmo.onell.problem.HammingDistance.*
import ru.ifmo.onell.problem.{LinearRandomIntegerWeights, OneMaxPerm, RandomPlanted3SAT}
import ru.ifmo.onell.util.Viridis
import ru.ifmo.onell.{Fitness, IterationLogger, Main}

import java.util.random.RandomGenerator
import scala.compiletime.uninitialized

object LambdaColorMap extends Main.Module:
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
    "  perm:om2 <options>: runs the bi-parametric experiments for OneMax on permutations.",
    "        Options are:",
    "             --n            <int>: the problem size",
    "             --runs         <int>: the number of runs for each λ",
    "             --max-lambda   <double>: the maximum λ to use",
    "             --lambda-power <double>: the multiplicative step for λ to use",
    "             --max-ell      <double>: the maximum L to use",
    "             --ell-power    <double>: the multiplicative step for L to use",
    "             --out-dir      <string>: the output directory to contain the data",
  ) ++ AlgorithmCodeNames.parserDescriptionForOnePlusLambdaLambdaGenerators("--tuning")

  override def moduleMain(args: Array[String]): Unit = args(0) match
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
    case "perm:om2" =>
      collect4DPlotsPerm(
        n = args.getOption("--n").toInt,
        runs = args.getOption("--runs").toInt,
        lambdaPower = args.getOption("--lambda-power").toDouble,
        maxLambda = args.getOption("--max-lambda").toDouble,
        ellPower = args.getOption("--ell-power").toDouble,
        maxEll = args.getOption("--max-ell").toDouble,
        outDir = args.getOption("--out-dir"))

  private class HammingImprovementStatistics(val size: Int):
    private val hammingCounts, hammingIncrements = Array.ofDim[Long](size + 1)
    private val countIncrements = Array.ofDim[Long](6)

    def consume(distance: Int, evaluations: Long, increment: Long): Unit = synchronized:
      hammingCounts(distance) += evaluations
      hammingIncrements(distance) += increment
      countIncrements(math.min(5, increment).toInt) += 1

    def incrementStats: String = s"1: ${countIncrements(1)}, 2: ${countIncrements(2)}, 3: ${countIncrements(3)}, 4: ${countIncrements(4)}, 5+: ${countIncrements(5)}"

    def extract(target: Array[Double]): Unit =
      for i <- 1 until target.length do
        target(i) = hammingIncrements(i).toDouble / hammingCounts(i)
  end HammingImprovementStatistics

  private class HammingImprovementCollector[F: Ordering as OF](stats: HammingImprovementStatistics) 
  extends IterationLogger[FAHD[F]]:
    private var lastEvaluations = 0L
    private var lastFitness: F = uninitialized
    private var lastDistance = -1

    override def logIteration(evaluations: Long, fitness: FAHD[F]): Unit =
      if evaluations == 1 then // start
        lastEvaluations = 1
        lastDistance = fitness.distance
        lastFitness = fitness.fitness
      else if OF.compare(fitness.fitness, lastFitness) > 0 then
        stats.consume(lastDistance, evaluations - lastEvaluations, lastDistance - fitness.distance)
        lastEvaluations = evaluations
        lastDistance = fitness.distance
        lastFitness = fitness.fitness
  end HammingImprovementCollector

  private class PermImprovementCollector(stats: HammingImprovementStatistics) extends IterationLogger[Int]:
    private var lastEvaluations = 0L
    private var lastDistance = -1

    override def logIteration(evaluations: Long, fitness: Int): Unit =
      val distance = stats.size - fitness
      if evaluations == 1 then // start
        lastEvaluations = 1
        lastDistance = distance
      else if distance < lastDistance then
        stats.consume(lastDistance, evaluations - lastEvaluations, lastDistance - distance)
        lastEvaluations = evaluations
        lastDistance = distance
  end PermImprovementCollector
  
  private def to3dPlotNumber(v: Double): String = String.format(Locale.US, "%.2e", v)

  private def collect3DPlots[F: Ordering](optimizerFromLambda: (Long => LambdaTuning) => OnePlusLambdaLambdaGA,
                                          problemInstanceGen: (Int, Long) => Fitness[Array[Boolean], F, Int],
                                          n: Int, runs: Int, lambdaPower: Double, file: String): Unit =
    val executor = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
    Using.resources(
      PrintWriter(file + ".txt.cmp"),
      PrintWriter(file + ".raw.cmp"),
    ): (out, raw) =>
      val rng = Random(9234352524211L)
      val maxLambda = (math.log(n) / 1.25 / math.log(lambdaPower)).toInt
      val arrays = Array.ofDim[Double](maxLambda, n / 2 + 1)

      def lambdaGenFun(lg: Int): Double = math.pow(lambdaPower, lg)

      for 
        lambdaGen <- arrays.indices
        lambda = lambdaGenFun(lambdaGen) 
      do
        val oll = optimizerFromLambda(fixedLambda(lambda))
        val logger = HammingImprovementStatistics(n)

        def newCallable(): Callable[Unit] = () => oll.optimize(
          problemInstanceGen(n, rng.nextLong()).withHammingDistanceTracking,
          new HammingImprovementCollector[F](logger))

        executor.invokeAll((0 until runs).map(_ => newCallable()).asJava).forEach(_.get())
        logger.extract(arrays(lambdaGen))
        println(s"[$file]: lambda $lambda done")
      end for
      
      raw.println(s"lambda power $lambdaPower")
      raw.println("distance offset 1")
      for a <- arrays do raw.println(a.view.slice(1, n / 2 + 1).map(to3dPlotNumber).mkString(" "))

      for dist <- 1 to n / 2 do
        val maxAcross = arrays.view.map(_(dist)).max
        for y <- arrays.indices do
          arrays(y)(dist) /= maxAcross

      out.println(s"lambda power $lambdaPower")
      out.println("distance offset 1")
      for a <- arrays do out.println(a.view.slice(1, n / 2 + 1).map(to3dPlotNumber).mkString(" "))

    executor.shutdown()
    executor.awaitTermination(365, TimeUnit.DAYS)

  private def collect3DPlots[F: Ordering](problemInstanceGen: (Int, Long) => Fitness[Array[Boolean], F, Int],
                                          n: Int, runs: Int, lambdaPower: Double, tuningMask: String, filePrefix: String): Unit =
    for (algFun, code) <- AlgorithmCodeNames.parseOnePlusLambdaLambdaGenerators(tuningMask) do
      collect3DPlots(algFun, problemInstanceGen, n, runs, lambdaPower, s"$filePrefix-$code")

  private def collect3DPlotsPerm(optimizerFromLambda: (Long => LambdaTuning) => OnePlusLambdaLambdaGA,
                                 n: Int, runs: Int, lambdaPower: Double, maxLambda: Double, file: String): Unit =
    val executor = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
    Using.resources(
      new PrintWriter(file + ".txt.cmp"),
      new PrintWriter(file + ".raw.cmp"),
    ): (out, raw) =>
      val maxLambdaIndex = 1 + (math.log(maxLambda) / math.log(lambdaPower)).toInt
      val arrays = Array.ofDim[Double](maxLambdaIndex, n + 1)

      def lambdaGenFun(lg: Int): Double = math.pow(lambdaPower, lg)

      for 
        lambdaGen <- arrays.indices
        lambda = lambdaGenFun(lambdaGen) 
      do
        val fun = OneMaxPerm(n)
        val oll = optimizerFromLambda(fixedLambda(lambda))
        val logger = HammingImprovementStatistics(n)

        def newCallable(): Callable[Unit] = () => oll.optimize(fun, PermImprovementCollector(logger))

        executor.invokeAll((0 until runs).map(_ => newCallable()).asJava).forEach(_.get())
        logger.extract(arrays(lambdaGen))
        println(s"[$file]: lambda $lambda done, improvement stats: ${logger.incrementStats}")
      end for
      
      raw.println(s"lambda power $lambdaPower")
      raw.println(s"distance offset 2")
      for a <- arrays do raw.println(a.view.slice(2, n + 1).map(to3dPlotNumber).mkString(" "))

      for dist <- 2 to n do
        val maxAcross = arrays.view.map(_ (dist)).max
        for y <- arrays.indices do
          arrays(y)(dist) /= maxAcross

      out.println(s"lambda power $lambdaPower")
      out.println(s"distance offset 2")
      for a <- arrays do out.println(a.view.slice(2, n + 1).map(to3dPlotNumber).mkString(" "))

    executor.shutdown()
    executor.awaitTermination(365, TimeUnit.DAYS)

  private def collect3DPlotsPerm(n: Int, runs: Int, lambdaPower: Double, maxLambda: Double,
                                 tuningMask: String, filePrefix: String): Unit =
    for (algFun, code) <- AlgorithmCodeNames.parseOnePlusLambdaLambdaGenerators(tuningMask) do
      collect3DPlotsPerm(algFun, n, runs, lambdaPower, maxLambda, s"$filePrefix-$code")

  private def samplePopSize(value: Double, rng: RandomGenerator): Int =
    val floor = math.floor(value).toInt
    val ceil = math.ceil(value).toInt
    if floor == ceil || rng.nextDouble() < value - floor then ceil else floor

  private class MyParamController(nChanges: Long, lambda: Double, ell: Double) extends OnePlusLambdaLambdaGA.ParameterController:
    private val ellDistribution = BinomialDistribution.resampling(nChanges, ell / nChanges)
    override def receiveFeedback(budgetSpent: Long, childToParentComparison: Int): Unit = ()
    override def getParameters(rng: RandomGenerator): OnePlusLambdaLambdaGA.IterationParameters =
      val nOfChanges = ellDistribution.sample(rng)
      val crossDist = BinomialDistribution.resampling(nOfChanges, 1.0 / nOfChanges)
      OnePlusLambdaLambdaGA.IterationParameters(firstPopulationSize = samplePopSize(lambda, rng),
                                                secondPopulationSize = samplePopSize(ell, rng),
                                                numberOfChangesInEachMutant = nOfChanges,
                                                numberOfChangesInCrossoverOffspring = crossDist.sample(rng))

  private def collect4DPlotsPerm(n: Int, runs: Int, lambdaPower: Double, maxLambda: Double,
                                 ellPower: Double, maxEll: Double, outDir: String): Unit =
    val executor = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
    val maxLambdaIndex = 1 + (math.log(maxLambda) / math.log(lambdaPower)).toInt
    val maxEllIndex = 1 + (math.log(maxEll) / math.log(ellPower)).toInt
    val arrays = Array.ofDim[Double](maxLambdaIndex, maxEllIndex, n + 1)

    for 
      lambdaGen <- 0 until maxLambdaIndex
      ellGen <- 0 until maxEllIndex
    do
      val lambda = math.pow(lambdaPower, lambdaGen)
      val ell = math.pow(ellPower, ellGen)
      val fun = OneMaxPerm(n)
      val oll = OnePlusLambdaLambdaGA(nChanges => MyParamController(nChanges, lambda, ell), OnePlusLambdaLambdaGA.BehaviorForGoodMutant.SkipCrossover)
      val logger = HammingImprovementStatistics(n)

      def newCallable(): Callable[Unit] = () => oll.optimize(fun, PermImprovementCollector(logger))

      executor.invokeAll((0 until runs).map(_ => newCallable()).asJava).forEach(_.get())
      logger.extract(arrays(lambdaGen)(ellGen))
      println(s"Config done: lambda $lambda (${lambdaGen + 1} / $maxLambdaIndex), ell $ell (${ellGen + 1} / $maxEllIndex)")

    val outDistanceFmt = s"%0${n.toString.length}d"
    new File(outDir).mkdirs()
    for distance <- 2 to n do
      val distanceStr = outDistanceFmt.format(distance)
      val map = Array.tabulate(maxLambdaIndex, maxEllIndex)((i, j) => arrays(i)(j)(distance))
      val max = map.view.map(_.max).max
      Using.resources(PrintWriter(s"$outDir/$distanceStr.raw"), PrintWriter(s"$outDir/$distanceStr.scl")): (raw, scl) =>
        raw.println(s"lambda power $lambdaPower")
        raw.println(s"ell power $ellPower")
        for m <- map do raw.println(m.view.map(to3dPlotNumber).mkString(" "))

        scl.println(s"lambda power $lambdaPower")
        scl.println(s"ell power $ellPower")
        for m <- map do scl.println(m.view.map(v => to3dPlotNumber(v / max)).mkString(" "))

        val img = BufferedImage(maxLambdaIndex, maxEllIndex, BufferedImage.TYPE_INT_RGB)
        for 
          i <- 0 until maxLambdaIndex
          j <- 0 until maxEllIndex
        do img.setRGB(i, maxEllIndex - 1 - j, Viridis(map(i)(j) / max))
        ImageIO.write(img, "png", File(s"$outDir/$distanceStr.png"))

    executor.shutdown()
    executor.awaitTermination(365, TimeUnit.DAYS)

  extension (args: Array[String])
    private def getOption(option: String): String =
      val index = args.indexOf(option)
      if index < 0 then throw IllegalArgumentException(s"No option '$option' is given")
      if index + 1 == args.length then throw IllegalArgumentException(s"Option '$option' should have an argument")
      args(index + 1)
  
