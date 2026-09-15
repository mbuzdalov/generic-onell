package ru.ifmo.onell.main

import java.io.PrintWriter
import java.util.Locale
import java.util.concurrent.atomic.AtomicLongArray

import scala.language.postfixOps
import scala.math.BigDecimal
import scala.util.Using

import ru.ifmo.onell.algorithm.oll.CompatibilityLayer.{createOnePlusLambdaLambdaGA, powerLawLambda}
import ru.ifmo.onell.problem.OneMax
import ru.ifmo.onell.util.par.{Executor, Multiplexer, ParallelExecutor}
import ru.ifmo.onell.{IterationLogger, Main}

object FixedTargetLambda extends Main.Module:
  override def name: String = "ft-lambda"
  override def shortDescription: String = "Runs experiments about fixed-target performance of the (1+(λ,λ)) GA"
  override def longDescription: Seq[String] = Seq(
    "Runs experiments about fixed-target performance of the heavy-tailed (1+(λ,λ)) GA on OneMax.",
    "The parameters are:",
    "  --n            <int>: the problem size",
    "  --beta-min  <double>: the minimal beta (the heavy-tailed distribution parameter) to test",
    "  --beta-max  <double>: the maximal beta to test",
    "  --beta-step <double>: the step for betas to test",
    "  --max-dist     <int>: the maximal distance from the optimum treated as a target",
    "  --runs         <int>: the number of independent runs for each configuration",
    "  --threads      <int>: the number of threads to use (less than one: all available threads)",
    "  --out     <filename>: the output filename to place the results"
  )

  private class FTLoggerStorage(val problemSize: Int, val algorithmName: String):
    private val collector = AtomicLongArray(problemSize + 1)
    private val collectorSq = AtomicLongArray(problemSize + 1)
    private val nEvents = AtomicLongArray(problemSize + 1)

    def getResult(index: Int): (Double, Double) =
      val sum = collector.get(index).toDouble
      val sumSq = collectorSq.get(index).toDouble
      val n = nEvents.get(index).toDouble
      val avg = sum / n
      val std = math.sqrt((sumSq / n - avg * avg) * n / (n - 1))
      (avg, std)

    def incCollections(minFitness: Int, maxFitness: Int, ev: Long): Unit =
      for index <- minFitness to maxFitness do
        collector.getAndAdd(index, ev)
        collectorSq.getAndAdd(index, ev * ev)
        nEvents.incrementAndGet(index)

  private class FixedTargetLogger(storage: FTLoggerStorage) extends IterationLogger[Int]:
    private var lastFitness = -1
    override def logIteration(evaluations: Long, fitness: Int): Unit =
      if evaluations == 1 then
        storage.incCollections(0, fitness, 1)
        lastFitness = fitness
      else if fitness > lastFitness then
        storage.incCollections(lastFitness + 1, fitness, evaluations)
        lastFitness = fitness

  private class Context(n: Int, nRuns: Int, nThreads: Int):
    def run(fun: (Executor[Unit], Int) => Unit): Unit =
      Using.resource(ParallelExecutor[Unit](nThreads)): scheduler =>
        val multiplexer = Multiplexer(scheduler, nRuns)
        fun(multiplexer, n)

  override def moduleMain(args: Array[String]): Unit =
    runForManyTargets(parseContext(args),
      BigDecimal(args.getOption("--beta-min")) to
      BigDecimal(args.getOption("--beta-max")) by
      BigDecimal(args.getOption("--beta-step")),
      args.getOption("--max-dist").toInt,
      args.getOption("--out")
    )

  private def runForManyTargets(context: Context, betaValues: Seq[BigDecimal], maxDistance: Int, out: String): Unit =
    val algorithms = for value <- betaValues yield
      val algo = createOnePlusLambdaLambdaGA(powerLawLambda(value.toDouble), 'R', "RL", 'C', 'D')
      ("%.02f".formatLocal(Locale.US, value), algo)

    val loggerBuffer = IndexedSeq.newBuilder[FTLoggerStorage]

    context.run: (scheduler, n) =>
      for (name, alg) <- algorithms do
        val logger = FTLoggerStorage(n, name)
        loggerBuffer += logger
        scheduler.addTask(alg.optimize(OneMax(n), FixedTargetLogger(logger)))

    val loggers = loggerBuffer.result().sortBy(_.algorithmName)
    Using.resource(PrintWriter(out)): pw =>
      pw.println(loggers.map(l => s"m${l.algorithmName},d${l.algorithmName}").mkString("k,", ",", ""))
      for pow <- 0 to 30 do
        val dist = 1 << pow
        if dist <= maxDistance then
          pw.println(loggers.map(l => l.getResult(l.problemSize - dist) match 
                                  case (a, b) => s"$a,$b"
                                ).mkString(s"$dist,", ",", ""))
  end runForManyTargets

  extension (args: Array[String])
    private def getOption(option: String): String =
      val index = args.indexOf(option)
      if index < 0 then throw IllegalArgumentException(s"No option '$option' is given")
      if index + 1 == args.length then throw IllegalArgumentException(s"Option '$option' should have an argument")
      args(index + 1)

  private def parseContext(args: Array[String]): Context = Context(
    n        = args.getOption("--n").toInt,
    nRuns    = args.getOption("--runs").toInt,
    nThreads = args.getOption("--threads").toInt,
  )
