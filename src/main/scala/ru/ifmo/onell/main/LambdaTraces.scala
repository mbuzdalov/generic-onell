package ru.ifmo.onell.main

import java.io.PrintWriter
import java.util.Random
import java.util.concurrent.ThreadLocalRandom

import scala.util.Using

import ru.ifmo.onell.{Fitness, IterationLogger, Main}
import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA
import ru.ifmo.onell.algorithm.oll.CompatibilityLayer._
import ru.ifmo.onell.main.util.AlgorithmCodeNames
import ru.ifmo.onell.problem.HammingDistance._
import ru.ifmo.onell.problem.{LinearRandomIntegerWeights, RandomPlanted3SAT}
import ru.ifmo.onell.util.Specialization

object LambdaTraces extends Main.Module {
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

  override def moduleMain(args: Array[String]): Unit = {
    val weight = args.getOption("--problem")
    if (weight == "SAT")
      collectTraces((n, seed) => new RandomPlanted3SAT(n, (4 * n * math.log(n)).toInt, RandomPlanted3SAT.EasyGenerator, seed),
                    n = args.getOption("--n").toInt,
                    runs = args.getOption("--runs").toInt,
                    tuningMask = args.getOption("--tuning"),
                    filePrefix = args.getOption("--out-prefix"))
    else
      collectTraces((n, seed) => new LinearRandomIntegerWeights(n, weight.toInt, seed),
                    n = args.getOption("--n").toInt,
                    runs = args.getOption("--runs").toInt,
                    tuningMask = args.getOption("--tuning"),
                    filePrefix = args.getOption("--out-prefix"))
  }

  private class LoggerWithLambdaProxy[
    @specialized(Specialization.fitnessSpecialization) F
  ](maxDistanceToLog: Int)(implicit fitness2long: F => Long)
    extends IterationLogger[FAHD[F]]
  {
    private[this] var lastLambda: Double = _
    private[this] var lastFitness: F = _
    private[this] val builder = new StringBuilder
    private[this] var distanceToWrite, lastDistance: Int = -1

    override def logIteration(evaluations: Long, fitness: FAHD[F]): Unit =
      if (evaluations == 1) {
        lastFitness = fitness.fitness
        lastLambda = 1.0
        if (fitness.distance <= maxDistanceToLog)
          builder.append("(").append(fitness.distance).append(",").append(lastLambda).append(")")
        lastDistance = fitness.distance
      } else if (fitness.fitness > lastFitness) {
        if (lastDistance <= maxDistanceToLog)
          builder.append("(").append(lastDistance).append(",").append(lastLambda).append(")")
        lastFitness = fitness.fitness
        distanceToWrite = fitness.distance
        lastDistance = fitness.distance
      }

    def result(): String = {
      val result = builder.result()
      builder.clear()
      result
    }

    def attachedTuning(realTuning: Long => LambdaTuning)(size: Long): LambdaTuning = new LambdaTuning {
      private[this] val delegate = realTuning(size)
      override def lambda(rng: ThreadLocalRandom): Double = {
        lastLambda = delegate.lambda(rng)
        if (distanceToWrite > 0) {
          if (distanceToWrite <= maxDistanceToLog)
            builder.append("(").append(distanceToWrite).append(",").append(lastLambda).append(")")
          distanceToWrite = -1
        }
        lastLambda
      }

      override def notifyChildIsBetter(budgetSpent: Long): Unit = delegate.notifyChildIsBetter(budgetSpent)
      override def notifyChildIsEqual(budgetSpent: Long): Unit = delegate.notifyChildIsEqual(budgetSpent)
      override def notifyChildIsWorse(budgetSpent: Long): Unit = delegate.notifyChildIsWorse(budgetSpent)
    }
  }

  //noinspection SameParameterValue: IDEA wrongly reports `file` to have the same parameter value for interpolated arg
  private def collectTraces[
    @specialized(Specialization.fitnessSpecialization) F
  ](algorithm: (Long => LambdaTuning) => OnePlusLambdaLambdaGA,
    problemInstanceGen: (Int, Long) => Fitness[Array[Boolean], F, Int],
    n: Int, runs: Int, file: String)
   (implicit fitness2long: F => Long): Unit = {
    Using.resource(new PrintWriter(file)) { out =>
      val rng = new Random(n * 234234 + runs * 912645 + file.hashCode)
      val logger = new LoggerWithLambdaProxy[F](n / 2)
      for (_ <- 0 until runs) {
        val problem = problemInstanceGen(n, rng.nextLong()).withHammingDistanceTracking
        val oll = algorithm(logger.attachedTuning(defaultOneFifthLambda))
        oll.optimize(problem, logger)
        out.println("\\addplot[black,update limits=false] coordinates {" + logger.result() + "};")
      }
    }
  }

  private def collectTraces[
    @specialized(Specialization.fitnessSpecialization) F
  ](problemInstanceGen: (Int, Long) => Fitness[Array[Boolean], F, Int],
    n: Int, runs: Int, tuningMask: String, filePrefix: String)
   (implicit fitness2long: F => Long): Unit =
    for ((algFun, code) <- AlgorithmCodeNames.parseOnePlusLambdaLambdaGenerators(tuningMask))
      collectTraces(algFun, problemInstanceGen, n, runs, s"$filePrefix-$code.tex")

  private implicit class Options(val args: Array[String]) extends AnyVal {
    def getOption(option: String): String = {
      val index = args.indexOf(option)
      if (index < 0) throw new IllegalArgumentException(s"No option '$option' is given")
      if (index + 1 == args.length) throw new IllegalArgumentException(s"Option '$option' should have an argument")
      args(index + 1)
    }
  }
}
