package ru.ifmo.onell.main

import java.io.PrintWriter
import java.util.Random
import scala.util.Using
import ru.ifmo.onell.{Fitness, IterationLogger, Main}
import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA
import ru.ifmo.onell.algorithm.oll.CompatibilityLayer.*
import ru.ifmo.onell.main.util.AlgorithmCodeNames
import ru.ifmo.onell.problem.HammingDistance.*
import ru.ifmo.onell.problem.{LinearRandomIntegerWeights, RandomPlanted3SAT}

import java.util.random.RandomGenerator
import scala.compiletime.uninitialized

object LambdaTraces extends Main.Module:
  override def name: String = "lambda-traces"

  override def shortDescription: String = "Runs experiments on how the parameter λ changes over time"

  override def longDescription: Seq[String] = Seq(
    "Runs experiments on how the parameter λ changes over time.",
    "The experiments are done for either linear functions with random",
    "integer weights, or easy random maximum satisfiability problems, on bit strings. The options are:",
    "  --n             <int>: the problem size",
    "  --runs          <int>: the number of runs",
    "  --problem <int|'SAT'>: either the maximum weight or 'SAT'",
    "  --tuning     <string>: the algorithm tunings to use",
    "  --out-prefix <string>: the prefix of the output files to use"
  ) ++ AlgorithmCodeNames.parserDescriptionForOnePlusLambdaLambdaGenerators("--tuning")

  override def moduleMain(args: Array[String]): Unit =
    val weight = args.getOption("--problem")
    if weight == "SAT" then
      collectTraces((n, seed) => RandomPlanted3SAT(n, (4 * n * math.log(n)).toInt, RandomPlanted3SAT.EasyGenerator, seed),
                    n = args.getOption("--n").toInt,
                    runs = args.getOption("--runs").toInt,
                    tuningMask = args.getOption("--tuning"),
                    filePrefix = args.getOption("--out-prefix"))
    else
      collectTraces((n, seed) => LinearRandomIntegerWeights(n, weight.toInt, seed),
                    n = args.getOption("--n").toInt,
                    runs = args.getOption("--runs").toInt,
                    tuningMask = args.getOption("--tuning"),
                    filePrefix = args.getOption("--out-prefix"))

  private class LoggerWithLambdaProxy[F: Ordering as FO](maxDistanceToLog: Int) extends IterationLogger[FAHD[F]]:
    private var lastLambda: Double = uninitialized
    private var lastFitness: F = uninitialized
    private val builder = StringBuilder()
    private var distanceToWrite, lastDistance: Int = -1

    override def logIteration(evaluations: Long, fitness: FAHD[F]): Unit =
      if evaluations == 1 then
        lastFitness = fitness.fitness
        lastLambda = 1.0
        if fitness.distance <= maxDistanceToLog then
          builder.append("(").append(fitness.distance).append(",").append(lastLambda).append(")")
        lastDistance = fitness.distance
      else if FO.compare(fitness.fitness, lastFitness) > 0 then
        if lastDistance <= maxDistanceToLog then
          builder.append("(").append(lastDistance).append(",").append(lastLambda).append(")")
        lastFitness = fitness.fitness
        distanceToWrite = fitness.distance
        lastDistance = fitness.distance

    def result(): String =
      val result = builder.result()
      builder.clear()
      result

    def attachedTuning(realTuning: Long => LambdaTuning)(size: Long): LambdaTuning = new LambdaTuning:
      private val delegate = realTuning(size)
      override def lambda(rng: RandomGenerator): Double =
        lastLambda = delegate.lambda(rng)
        if distanceToWrite > 0 then
          if distanceToWrite <= maxDistanceToLog then
            builder.append("(").append(distanceToWrite).append(",").append(lastLambda).append(")")
          distanceToWrite = -1
        lastLambda

      override def notifyChildIsBetter(budgetSpent: Long): Unit = delegate.notifyChildIsBetter(budgetSpent)
      override def notifyChildIsEqual(budgetSpent: Long): Unit = delegate.notifyChildIsEqual(budgetSpent)
      override def notifyChildIsWorse(budgetSpent: Long): Unit = delegate.notifyChildIsWorse(budgetSpent)
  end LoggerWithLambdaProxy
  
  private def collectTraces[F: Ordering](algorithm: (Long => LambdaTuning) => OnePlusLambdaLambdaGA,
                                         problemInstanceGen: (Int, Long) => Fitness[Array[Boolean], F, Int],
                                         n: Int, runs: Int, file: String): Unit = 
    Using.resource(PrintWriter(file)): out =>
      val rng = Random(n * 234234 + runs * 912645 + file.hashCode)
      val logger = LoggerWithLambdaProxy[F](n / 2)
      for _ <- 0 until runs do
        val problem = problemInstanceGen(n, rng.nextLong()).withHammingDistanceTracking
        val oll = algorithm(logger.attachedTuning(defaultOneFifthLambda))
        oll.optimize(problem, logger)
        out.println("\\addplot[black,update limits=false] coordinates {" + logger.result() + "};")

  private def collectTraces[F: Ordering](problemInstanceGen: (Int, Long) => Fitness[Array[Boolean], F, Int],
                                         n: Int, runs: Int, tuningMask: String, filePrefix: String): Unit =
    for (algFun, code) <- AlgorithmCodeNames.parseOnePlusLambdaLambdaGenerators(tuningMask) do
      collectTraces(algFun, problemInstanceGen, n, runs, s"$filePrefix-$code.tex")

  extension (args: Array[String])
    private def getOption(option: String): String =
      val index = args.indexOf(option)
      if index < 0 then throw IllegalArgumentException(s"No option '$option' is given")
      if index + 1 == args.length then throw IllegalArgumentException(s"Option '$option' should have an argument")
      args(index + 1)
