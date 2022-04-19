package ru.ifmo.onell.main

import java.io.PrintWriter
import java.util.concurrent.atomic.{AtomicInteger, AtomicLongArray}

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
    "Runs experiments about fixed-target performance of the (1+(λ,λ)) GA on OneMax.",
    "The parameters are:",
    "  --from     <int>: the minimum power of two for the problem size",
    "  --to       <int>: the maximum power of two for the problem size",
    "  --share <double>: the started value of the fixed-target log",
    "  --runs     <int>: the number of independent runs for each configuration",
    "  --threads  <int>: the number of threads to use (less than one: all available threads)",
  )

  private class FTLoggerStorage(val problemSize: Int, val algName: String) {
    private[this] val collector = new AtomicLongArray(problemSize + 1)
    private[this] val collectorSq = new AtomicLongArray(problemSize + 1)

    def getJsonStrPart(index: Int, runs: Int): String = {
      val avg = collector.get(index).toDouble / runs
      s"""{"n":$problemSize,"algorithm":\"$algName\","runtime":${avg / problemSize}}"""
    }

    def getStr(index: Int, runs: Int): String = {
      val avg = collector.get(index).toDouble / runs
      val std = math.sqrt((collectorSq.get(index).toDouble / runs - avg * avg) * runs / (runs - 1))
      s"${index.toDouble}\t$avg\t$std"
    }

    def incCollections(minFitness: Int, maxFitness: Int, ev: Long): Unit = {
      for (index <- minFitness to maxFitness) {
        collector.getAndAdd(index, ev)
        collectorSq.getAndAdd(index, ev * ev)
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

  private class Context(val powers: Range, val betaFrom: Double, val betaTo: Double, val share: Double, val nRuns: Int, nThreads: Int) {
    def run(fun: (Executor[Unit], Int) => Array[FTLoggerStorage]): Array[Array[FTLoggerStorage]] = {
      var loggers = new Array[Array[FTLoggerStorage]](0)
      Using.resource(new ParallelExecutor[Unit](nThreads)) { scheduler =>
        val multiplexer = new Multiplexer(scheduler, nRuns)
        for (p <- powers) {
          loggers :+= fun(multiplexer, 1 << p)
        }
      }
      loggers
    }
  }

  override def moduleMain(args: Array[String]): Unit = {
    runForSqrt(parseContext(args))
  }

  private def DumpJsonPart(result_loggers: Array[Array[FTLoggerStorage]], algorithms: Seq[String],
                           func: Int => Int,
                           runs: Int, prefix: String): Unit = {
    Using.resource(new PrintWriter(s"part_jsons/part_$prefix.json")) { out => {
      out.print("[")
      var is_first = true
      for (i <- algorithms.indices) {
        for (loggers <- result_loggers) {
          val cur_logger = loggers(i)
          val ft_value = cur_logger.problemSize - func(cur_logger.problemSize)
          if (!is_first) {
            out.print(",")
          } else {
            is_first = false
          }
          out.println(cur_logger.getJsonStrPart(ft_value, runs))
        }
      }
      out.println("]")
    }}
  }

  private def runForSqrt(context: Context): Unit = {
    val algorithms = for (value <- context.betaFrom to context.betaTo by 0.05d) yield {
      println(s"Fix beta = $value")
      s"pow($value)" -> createOnePlusLambdaLambdaGA(powerLawLambda(value.toDouble), 'R', "RL", 'C', 'D')
    }

    val alg_names = algorithms.map(obj => obj._1)
    val done_tasks = new AtomicInteger(0)
    val total_tasks = context.nRuns * algorithms.length * context.powers.length

    val fts_funcs = Seq(
      "sqrt(n)" -> ((n: Int) => math.sqrt(n.toDouble).ceil.toInt),
      "sqrt^3(n)" -> ((n: Int) => math.pow(n.toDouble, 1.0 / 3).ceil.toInt),
      "ln(n)" -> ((n: Int) => math.log(n.toDouble).ceil.toInt),
      "sqrt(n)ln(n)" -> ((n: Int) => (math.sqrt(n.toDouble) * math.log(n.toDouble)).ceil.toInt),
      "sqrt(n ln(n))" -> ((n: Int) => math.sqrt(math.log(n.toDouble) * n).ceil.toInt),
    )

    val result_loggers = context.run { (scheduler, n) =>
      var loggers = new Array[FTLoggerStorage](0)
      for ((name, alg) <- algorithms) {
        val ft_logger = new FTLoggerStorage(n, name)
        scheduler addTask {
          val res = alg.optimize(new OneMax(n, 0), new FixedTargetLogger(ft_logger))
          println(s"Finished algo $name, problem size $n, time $res")

          val done = done_tasks.incrementAndGet()
          if (done % 10 == 0) {
            println(s"Done: ${100.0 * done.toDouble / total_tasks}% ($done out of $total_tasks)")
          }
        }
        loggers :+= ft_logger
        println(s"Algorithm $name pushed to queue")
      }
      println(s"Finished scheduling, loggers size ${loggers.length}, problem size $n")
      loggers
    }

    println(s"Finished processing, result loggers size ${result_loggers.length}")

    for (i <- algorithms.indices) {
      for (loggers <- result_loggers) {
        val cur_logger = loggers(i)
        Using.resource(new PrintWriter(s"data/${algorithms(i)._1}_${cur_logger.problemSize}.dat")) { out =>
          out.println(s"${cur_logger.problemSize}\t${context.nRuns}")
          for (ft_value <- 0 to cur_logger.problemSize) {
            out.println(cur_logger.getStr(ft_value, context.nRuns))
          }
        }
      }
    }

    for ((name, func) <- fts_funcs) {
      DumpJsonPart(result_loggers, alg_names, func, context.nRuns, name)
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
    powers   = args.getOption("--from").toInt to args.getOption("--to").toInt,
    betaFrom = args.getOption("--bfrom").toDouble,
    betaTo   = args.getOption("--bto").toDouble,
    share    = args.getOption("--share").toDouble,
    nRuns    = args.getOption("--runs").toInt,
    nThreads = args.getOption("--threads").toInt
  )
}
