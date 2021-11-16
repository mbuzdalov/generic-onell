package ru.ifmo.onell.main

import ru.ifmo.onell.{IterationLogger, Main}
import ru.ifmo.onell.algorithm.oll.CompatibilityLayer.{createOnePlusLambdaLambdaGA, defaultOneFifthLambda, fixedLogTowerLambda, logCappedOneFifthLambda, powerLawLambda}
import ru.ifmo.onell.problem.OneMax
import ru.ifmo.onell.util.par.{Executor, Multiplexer, ParallelExecutorFT}

import java.io.PrintWriter
import java.util.concurrent.atomic
import java.util.concurrent.atomic.AtomicLongArray
import scala.util.Using

object FixedTargetLambda extends Main.Module {
  override def name: String = "ft-lambda"
  override def shortDescription: String = "Runs experiments about fixed-target performance of the (1 + (λ, λ)) GA"
  override def longDescription: Seq[String] = Seq(
    "Runs experiments about fixed-target performance of the (1 + (λ, λ)) GA on OneMax.",
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

    def getJsonStr(index: Int, runs: Int): String = {
      val avg = collector.get(index).toDouble / runs
      val std = math.sqrt((collectorSq.get(index).toDouble / runs - avg * avg) * runs / (runs - 1))
      s"""{"n":$problemSize,"algorithm":\"$algName\","index":$index,"runtime":$avg,"std":$std}"""
    }

    def getTexStr(index: Int, runs: Int): String = {
      val avg = collector.get(index).toDouble / runs
      val std = math.sqrt((collectorSq.get(index).toDouble / runs - avg * avg) * runs / (runs - 1))
      s"(${index.toDouble / problemSize},$avg)+-(0,$std)"
    }

    def getStr(index: Int, runs: Int): String = {
      val avg = collector.get(index).toDouble / runs
      val std = math.sqrt((collectorSq.get(index).toDouble / runs - avg * avg) * runs / (runs - 1))
      s"${index.toDouble}\t$avg\t$std"
    }

    def incCollections(index: Int, ev: Long, sqev: Long): Unit = {
      collector.getAndAdd(index, ev)
      collectorSq.getAndAdd(index, sqev)
    }
  }

  private class FixedTargetLogger(storage: FTLoggerStorage) extends IterationLogger[Int] {
    private[this] var lastFitness = -1

    override def logIteration(evaluations: Long, fitness: Int): Unit = {
      if (evaluations == 1) {
          for (i <- 0 to fitness) {
            storage.incCollections(i, 1, 1)
          }
          lastFitness = fitness
      } else if (fitness > lastFitness) {
        val ev2 = evaluations * evaluations
        for (i <- lastFitness + 1 to fitness) {
          storage.incCollections(i, evaluations, ev2)
        }
        lastFitness = fitness
      }
    }
  }

  private class Context(powers: Range, val share: Double, val nRuns: Int, nThreads: Int) {
    def run(fun: (Executor[Unit], Int) => Array[FTLoggerStorage]): Array[Array[FTLoggerStorage]] = {
      var loggers = new Array[Array[FTLoggerStorage]](0)
      Using.resource(makeScheduler()) { scheduler =>
        val multiplexer = new Multiplexer(scheduler, nRuns)
        for (p <- powers) {
          loggers :+= fun(multiplexer, 1 << p)
        }
      }
      loggers
    }

    private def makeScheduler(): Executor[Unit] =
      new ParallelExecutorFT(nThreads)
  }

  override def moduleMain(args: Array[String]): Unit = {
    run(parseContext(args))
  }

  private def DumpTex(loggers: Array[FTLoggerStorage], algorithms: Seq[String], start: Int, runs: Int, prefix: String): Unit = {
    Using.resource(new PrintWriter(s"plots/${prefix}_${loggers.last.problemSize}.tex")) { tex_out => {
      for (i <- loggers.indices) {
        val cur_logger = loggers(i)
        tex_out.print("\\addplot+ plot[error bars/.cd, y dir=both, y explicit] coordinates {")
        for (percent_share <- 0 to 100) {
          val share = percent_share.toDouble / 100.0
          val diff = cur_logger.problemSize - start
          val ft_value = (share * diff + start).ceil.toInt
          tex_out.print(cur_logger.getTexStr(ft_value, runs))
        }
        tex_out.println("};")
        tex_out.println(s"\\addlegendentry{${algorithms(i)}};")
      }
    }
    }
  }

  private def DumpJson(loggers: Array[FTLoggerStorage], algorithms: Seq[String], start: Int, runs: Int, prefix: String): Unit = {
    Using.resource(new PrintWriter(s"jsons/${prefix}_${loggers.last.problemSize}.json")) { json_out => {
      json_out.print("[")
      var is_first: Boolean = true
      for (i <- loggers.indices) {
        val cur_logger = loggers(i)
        for (percent_share <- 0 to 100) {
          val share = percent_share.toDouble / 100.0
          val diff = cur_logger.problemSize - start
          val ft_value = (share * diff + start).ceil.toInt
          if (!is_first) {
            json_out.print(",")
          } else {
            is_first = false
          }
          json_out.println(cur_logger.getJsonStr(ft_value, runs))
        }
      }
      json_out.println("]")
    }
    }
  }

  private def run(context: Context): Unit = {
    val algorithms = Seq(
      ("λ<=n", "n", createOnePlusLambdaLambdaGA(defaultOneFifthLambda, 'R', "RL", 'C', 'D')),
      ("λ<=2ln n", "2ln n", createOnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'R', "RL", 'C', 'D')),
      ("λ~pow(2.1)", "pow(2.1)", createOnePlusLambdaLambdaGA(powerLawLambda(2.1), 'R', "RL", 'C', 'D')),
      ("λ~pow(2.2)", "pow(2.2)", createOnePlusLambdaLambdaGA(powerLawLambda(2.2), 'R', "RL", 'C', 'D')),
      ("λ~pow(2.3)", "pow(2.3)", createOnePlusLambdaLambdaGA(powerLawLambda(2.3), 'R', "RL", 'C', 'D')),
      ("λ~pow(2.4)", "pow(2.4)", createOnePlusLambdaLambdaGA(powerLawLambda(2.4), 'R', "RL", 'C', 'D')),
      ("λ~pow(2.5)", "pow(2.5)", createOnePlusLambdaLambdaGA(powerLawLambda(2.5), 'R', "RL", 'C', 'D')),
      ("λ~pow(2.6)", "pow(2.6)", createOnePlusLambdaLambdaGA(powerLawLambda(2.6), 'R', "RL", 'C', 'D')),
      ("λ~pow(2.7)", "pow(2.7)", createOnePlusLambdaLambdaGA(powerLawLambda(2.7), 'R', "RL", 'C', 'D')),
      ("λ~pow(2.8)", "pow(2.8)", createOnePlusLambdaLambdaGA(powerLawLambda(2.8), 'R', "RL", 'C', 'D')),
      ("λ~pow(2.9)", "pow(2.9)", createOnePlusLambdaLambdaGA(powerLawLambda(2.9), 'R', "RL", 'C', 'D')),
      ("λ=fixed optimal", "fixed-optimal", createOnePlusLambdaLambdaGA(fixedLogTowerLambda, 'R', "RL", 'C', 'D')),
    )

    val alg_names = algorithms.map(obj => obj._1)

    val result_loggers = context.run { (scheduler, n) =>
      var loggers = new Array[FTLoggerStorage](0)
      for ((name, _, alg) <- algorithms) {
        val ft_logger = new FTLoggerStorage(n, name)
        scheduler addTask {
          alg.optimize(new OneMax(n), new FixedTargetLogger(ft_logger))
        }
        loggers :+= ft_logger
        println(s"Algorithm ${name} pushed to queue")
      }
      println(s"Finished scheduling, loggers size ${loggers.length}, problem size ${n}")
      loggers
    }

    println(s"Finished processing, result loggers size ${result_loggers.length}")

    for (i <- algorithms.indices) {
      for (loggers <- result_loggers) {
        val cur_logger = loggers(i)
        Using.resource(new PrintWriter(s"data/${algorithms(i)._2}_${cur_logger.problemSize}.dat")) { out => {
          out.println(s"${cur_logger.problemSize}\t${context.nRuns}")
          for (ft_value <- 0 to cur_logger.problemSize) {
            out.println(cur_logger.getStr(ft_value, context.nRuns))
          }
        }
        }
      }
    }

    for (loggers <- result_loggers) {
      val start_def = (loggers.last.problemSize * 0.5).ceil.toInt
      DumpTex(loggers, alg_names, start_def, context.nRuns, "full_runtime")
      val start = (loggers.last.problemSize * context.share).ceil.toInt
      DumpTex(loggers, alg_names, start, context.nRuns, "ft_runtime")

      DumpJson(loggers, alg_names, start_def, context.nRuns, "full_runtime")
      DumpJson(loggers, alg_names, start, context.nRuns, "ft_runtime")
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
    share    = args.getOption("--share").toDouble,
    nRuns    = args.getOption("--runs").toInt,
    nThreads = args.getOption("--threads").toInt
  )
}
