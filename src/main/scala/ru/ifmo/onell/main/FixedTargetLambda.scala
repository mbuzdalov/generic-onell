package ru.ifmo.onell.main

import java.util.Locale
import java.util.concurrent.atomic.AtomicLongArray

import scala.language.postfixOps
import scala.math.BigDecimal.double2bigDecimal
import scala.util.Using

import ru.ifmo.onell.algorithm.oll.CompatibilityLayer.{createOnePlusLambdaLambdaGA, powerLawLambda}
import ru.ifmo.onell.problem.OneMax
import ru.ifmo.onell.util.par.{Executor, Multiplexer, ParallelExecutor}
import ru.ifmo.onell.{IterationLogger, Main}

object FixedTargetLambda extends Main.Module {
  override def name: String = "ft-lambda"
  override def shortDescription: String = "Runs experiments about fixed-target performance of the (1+(λ,λ)) GA"
  override def longDescription: Seq[String] = Seq(
    "Runs experiments about fixed-target performance of the heavy-tailed (1+(λ,λ)) GA on OneMax.",
    "The parameters are:",
    "  --n         <int>: the problem size",
    "  --beta-min  <double>: the minimal beta (the heavy-tailed distribution parameter) to test",
    "  --beta-max  <double>: the maximal beta to test",
    "  --beta-step <double>: the step for betas to test",
    "  --max-dist  <int>: the maximal distance from the optimum treated as a target",
    "  --runs      <int>: the number of independent runs for each configuration",
    "  --threads   <int>: the number of threads to use (less than one: all available threads)",
  )

  private class FTLoggerStorage(val problemSize: Int, val algorithmName: String) {
    private[this] val collector = new AtomicLongArray(problemSize + 1)
    private[this] val collectorSq = new AtomicLongArray(problemSize + 1)
    private[this] val nEvents = new AtomicLongArray(problemSize + 1)

    def getResult(index: Int): (Double, Double) = {
      val sum = collector.get(index).toDouble
      val sumSq = collectorSq.get(index).toDouble
      val n = nEvents.get(index).toDouble
      val avg = sum / n
      val std = math.sqrt((sumSq / n - avg * avg) * n / (n - 1))
      (avg, std)
    }

    def incCollections(minFitness: Int, maxFitness: Int, ev: Long): Unit = {
      for (index <- minFitness to maxFitness) {
        collector.getAndAdd(index, ev)
        collectorSq.getAndAdd(index, ev * ev)
        nEvents.incrementAndGet(index)
      }
    }
  }

  private class FixedTargetLogger(storage: FTLoggerStorage) extends IterationLogger[Int] {
    private[this] var lastFitness = -1
    override def logIteration(evaluations: Long, fitness: Int): Unit = {
      if (evaluations == 1) {
        storage.incCollections(0, fitness, 1)
        lastFitness = fitness
      } else if (fitness > lastFitness) {
        storage.incCollections(lastFitness + 1, fitness, evaluations)
        lastFitness = fitness
      }
    }
  }

  private class Context(n: Int, nRuns: Int, nThreads: Int) {
    def run(fun: (Executor[Unit], Int) => Unit): Unit = {
      Using.resource(new ParallelExecutor[Unit](nThreads)) { scheduler =>
        val multiplexer = new Multiplexer(scheduler, nRuns)
        fun(multiplexer, n)
      }
    }
  }

  override def moduleMain(args: Array[String]): Unit = {
    runForManyTargets(parseContext(args),
      args.getOption("--beta-min").toDouble to
      args.getOption("--beta-max").toDouble by
      args.getOption("--beta-step").toDouble,
      args.getOption("--max-dist").toInt
    )
  }

  private def runForManyTargets(context: Context, betaValues: Seq[BigDecimal], maxDistance: Int): Unit = {
    val algorithms = for (value <- betaValues) yield {
      val algo = createOnePlusLambdaLambdaGA(powerLawLambda(value.toDouble), 'R', "RL", 'C', 'D')
      ("%.02f".formatLocal(Locale.US, value), algo)
    }

    val loggerBuffer = IndexedSeq.newBuilder[FTLoggerStorage]

    context.run { (scheduler, n) =>
      for ((name, alg) <- algorithms) {
        val logger = new FTLoggerStorage(n, name)
        loggerBuffer += logger
        scheduler addTask {
          alg.optimize(new OneMax(n), new FixedTargetLogger(logger))
        }
      }
    }

    val loggers = loggerBuffer.result().sortBy(_.algorithmName)
    println(loggers
      .map(l => s"m${l.algorithmName},d${l.algorithmName}")
      .mkString("k,", ",", ""))
    for (pow <- 0 to 30; dist = 1 << pow; if dist <= maxDistance) {
      println(loggers
        .map(l => l.getResult(l.problemSize - dist) match {
          case (a, b) => s"$a,$b"
        }).mkString(s"$dist,", ",", ""))
    }
  }

  private implicit class Options(val args: Array[String]) extends AnyVal {
    def getOption(option: String): String = {
      val index = args.indexOf(option)
      if (index < 0) throw new IllegalArgumentException(s"No option '$option' is given")
      if (index + 1 == args.length) throw new IllegalArgumentException(s"Option '$option' should have an argument")
      args(index + 1)
    }
  }

  private def parseContext(args: Array[String]): Context = new Context(
    n        = args.getOption("--n").toInt,
    nRuns    = args.getOption("--runs").toInt,
    nThreads = args.getOption("--threads").toInt,
  )
}
