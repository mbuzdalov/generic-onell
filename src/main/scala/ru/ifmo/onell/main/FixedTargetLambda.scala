package ru.ifmo.onell.main

import ru.ifmo.onell.algorithm.{OnePlusLambdaLambdaGA, OnePlusOneEA}
import ru.ifmo.onell.{IterationLogger, Main}
import ru.ifmo.onell.algorithm.oll.CompatibilityLayer.{createOnePlusLambdaLambdaGA, defaultOneFifthLambda, fixedLogTowerLambda, logCappedOneFifthLambda, powerLawLambda}
import ru.ifmo.onell.problem.OneMax
import ru.ifmo.onell.util.par.{Executor, Multiplexer, ParallelExecutorFT}

import java.io.PrintWriter
import java.util.concurrent.atomic
import java.util.concurrent.atomic.{AtomicInteger, AtomicLongArray}
import scala.language.postfixOps
import scala.math.BigDecimal.double2bigDecimal
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
      s"""{"n":$problemSize,"algorithm":\"$algName\","index":${problemSize - index},"runtime":$avg,"std":$std}"""
    }

    def getJsonStrPart(index: Int, runs: Int): String = {
      val avg = collector.get(index).toDouble / runs
      val std = math.sqrt((collectorSq.get(index).toDouble / runs - avg * avg) * runs / (runs - 1))
      s"""{"n":$problemSize,"algorithm":\"$algName\","runtime":${avg / problemSize}}"""
    }

    def getJsonDerStr(index: Int, runs: Int): String = {
      val index_prev = index// if (index == 0) index else index - 1
      val index_next = if (index == problemSize) index else index + 1
      val diff = index_next - index_prev

      val avg_prev = collector.get(index_prev).toDouble / runs
      val avg_next = collector.get(index_next).toDouble / runs

      s"""{"n":$problemSize,"algorithm":\"$algName\","index":${problemSize - index},"derivative":${(avg_next - avg_prev) / diff}}"""
    }

    def getTexStr(index: Int, runs: Int): String = {
      val avg = collector.get(index).toDouble / runs
      val std = math.sqrt((collectorSq.get(index).toDouble / runs - avg * avg) * runs / (runs - 1))
      s"(${index.toDouble / problemSize},$avg)+-(0,$std)"
    }

    def getTexDerStr(index: Int, runs: Int): String = {
      val index_prev = if (index == 0) index else index - 1
      val index_next = if (index == problemSize) index else index + 1
      val diff = index_next - index_prev

      val avg_prev = collector.get(index_prev).toDouble / runs
      val avg_next = collector.get(index_next).toDouble / runs

      s"(${index.toDouble / problemSize},${(avg_next - avg_prev) / diff})+-(0,0)"
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

  private class Context(val powers: Range, val beta_from: Double, val beta_to: Double, val share: Double, val nRuns: Int, nThreads: Int) {
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
    runForSqrt(parseContext(args))
  }

  private def DumpTex(loggers: Array[FTLoggerStorage], algorithms: Seq[String], start: Int, grain: Int, runs: Int, prefix: String, use_derivative: Boolean): Unit = {
    Using.resource(new PrintWriter(s"plots/${prefix}_${loggers.last.problemSize}.tex")) { tex_out => {
      for (i <- loggers.indices) {
        val cur_logger = loggers(i)
        tex_out.print("\\addplot+ plot[error bars/.cd, y dir=both, y explicit] coordinates {")
        for (percent_share <- 0 to grain) {
          val share = percent_share.toDouble / grain.toDouble
          val diff = cur_logger.problemSize - start
          val ft_value = (share * diff + start).ceil.toInt
          tex_out.print(if (use_derivative) cur_logger.getTexDerStr(ft_value, runs) else cur_logger.getTexStr(ft_value, runs))
        }
        tex_out.println("};")
        tex_out.println(s"\\addlegendentry{${algorithms(i)}};")
      }
    }
    }
  }

  private def DumpJson(loggers: Array[FTLoggerStorage], algorithms: Seq[String], start: Int, grain: Int, runs: Int, prefix: String, use_derivative: Boolean): Unit = {
    Using.resource(new PrintWriter(s"jsons/${prefix}_${loggers.last.problemSize}.json")) { json_out => {
      json_out.print("[")
      var is_first: Boolean = true
      for (i <- loggers.indices) {
        val cur_logger = loggers(i)
        for (percent_share <- 0 to grain) {
          val share = percent_share.toDouble / grain.toDouble
          val diff = cur_logger.problemSize - start
          val ft_value = (share * diff + start).ceil.toInt

          if (ft_value != cur_logger.problemSize) {
            if (!is_first) {
              json_out.print(",")
            } else {
              is_first = false
            }

            json_out.println(if (use_derivative) cur_logger.getJsonDerStr(ft_value, runs) else cur_logger.getJsonStr(ft_value, runs))
          }
        }
      }
      json_out.println("]")
    }
    }
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
    var algorithms = new Array[(String, OnePlusLambdaLambdaGA)](0)

    val values = context.beta_from to context.beta_to by 0.05d toArray

    for (value <- values) {
      println(s"Fix beta = ${value}")
    }

    for (value <- values) {
      algorithms :+= s"pow(${value})" -> createOnePlusLambdaLambdaGA(powerLawLambda(value.toDouble), 'R', "RL", 'C', 'D')
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
          println(s"Finished algo ${name}, problem size ${n}, time ${res}")

          val done = done_tasks.incrementAndGet()
          if (done % 10 == 0) {
            println(s"Done: ${100.0 * done.toDouble / total_tasks}% ($done out of $total_tasks)")
          }
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
        Using.resource(new PrintWriter(s"data/${algorithms(i)._1}_${cur_logger.problemSize}.dat")) { out => {
          out.println(s"${cur_logger.problemSize}\t${context.nRuns}")
          for (ft_value <- 0 to cur_logger.problemSize) {
            out.println(cur_logger.getStr(ft_value, context.nRuns))
          }
        }
        }
      }
    }

    for ((name, func) <- fts_funcs) {
      DumpJsonPart(result_loggers, alg_names.toIndexedSeq, func, context.nRuns, name)
    }
  }

  private def run(context: Context): Unit = {
    val algorithms = Seq(
//      ("λ<=n", "n", createOnePlusLambdaLambdaGA(defaultOneFifthLambda, 'R', "RL", 'C', 'D')),
//      ("λ<=2ln n", "2ln n", createOnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'R', "RL", 'C', 'D')),
//      ("RLS", "RLS", OnePlusOneEA.RLS),
//      ("(1+1) EA", "1+1", OnePlusOneEA.Resampling),
      ("λ~pow(2.1)", "pow(2.1)", createOnePlusLambdaLambdaGA(powerLawLambda(2.1), 'R', "RL", 'C', 'D')),
      ("λ~pow(2.2)", "pow(2.2)", createOnePlusLambdaLambdaGA(powerLawLambda(2.2), 'R', "RL", 'C', 'D')),
      ("λ~pow(2.3)", "pow(2.3)", createOnePlusLambdaLambdaGA(powerLawLambda(2.3), 'R', "RL", 'C', 'D')),
      ("λ~pow(2.4)", "pow(2.4)", createOnePlusLambdaLambdaGA(powerLawLambda(2.4), 'R', "RL", 'C', 'D')),
      ("λ~pow(2.5)", "pow(2.5)", createOnePlusLambdaLambdaGA(powerLawLambda(2.5), 'R', "RL", 'C', 'D')),
      ("λ~pow(2.6)", "pow(2.6)", createOnePlusLambdaLambdaGA(powerLawLambda(2.6), 'R', "RL", 'C', 'D')),
      ("λ~pow(2.7)", "pow(2.7)", createOnePlusLambdaLambdaGA(powerLawLambda(2.7), 'R', "RL", 'C', 'D')),
      ("λ~pow(2.8)", "pow(2.8)", createOnePlusLambdaLambdaGA(powerLawLambda(2.8), 'R', "RL", 'C', 'D')),
      ("λ~pow(2.9)", "pow(2.9)", createOnePlusLambdaLambdaGA(powerLawLambda(2.9), 'R', "RL", 'C', 'D')),
//      ("λ=fixed optimal", "fixed-optimal", createOnePlusLambdaLambdaGA(fixedLogTowerLambda, 'R', "RL", 'C', 'D')),
//      ("λ~pow(3)", "pow(3)", createOnePlusLambdaLambdaGA(powerLawLambda(4), 'R', "RL", 'C', 'D')),
//      ("λ~pow(3.5)", "pow(3.5)", createOnePlusLambdaLambdaGA(powerLawLambda(3.5), 'R', "RL", 'C', 'D')),
//      ("λ~pow(4)", "pow(4)", createOnePlusLambdaLambdaGA(powerLawLambda(4), 'R', "RL", 'C', 'D')),
//      ("λ~pow(4.5)", "pow(4.5)", createOnePlusLambdaLambdaGA(powerLawLambda(4.5), 'R', "RL", 'C', 'D')),
//      ("λ~pow(5)", "pow(5)", createOnePlusLambdaLambdaGA(powerLawLambda(5), 'R', "RL", 'C', 'D')),
//      ("λ~pow(7)", "pow(7)", createOnePlusLambdaLambdaGA(powerLawLambda(7), 'R', "RL", 'C', 'D')),
//      ("λ~pow(10)", "pow(10)", createOnePlusLambdaLambdaGA(powerLawLambda(10), 'R', "RL", 'C', 'D')),
//      ("λ~pow(15)", "pow(15)", createOnePlusLambdaLambdaGA(powerLawLambda(15), 'R', "RL", 'C', 'D')),
//      ("λ~pow(20)", "pow(20)", createOnePlusLambdaLambdaGA(powerLawLambda(20), 'R', "RL", 'C', 'D')),
    )

    val alg_names = algorithms.map(obj => obj._1)
    val done_tasks = new AtomicInteger(0)
    val total_tasks = context.nRuns * algorithms.length * context.powers.length

    val result_loggers = context.run { (scheduler, n) =>
      var loggers = new Array[FTLoggerStorage](0)
      for ((name, _, alg) <- algorithms) {
        val ft_logger = new FTLoggerStorage(n, name)
        scheduler addTask {
          alg.optimize(new OneMax(n), new FixedTargetLogger(ft_logger))
          val done = done_tasks.incrementAndGet()
          if (done % 10 == 0) {
            println(s"Done: ${100.0 * done.toDouble / total_tasks}%")
          }
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
      DumpTex(loggers, alg_names, start_def, 1000, context.nRuns, "full_runtime", false)
      val start = (loggers.last.problemSize * context.share).ceil.toInt
      DumpTex(loggers, alg_names, start, 1000, context.nRuns, "ft_runtime", false)

      DumpJson(loggers, alg_names, start_def, 1000, context.nRuns, "full_runtime", false)
      DumpJson(loggers, alg_names, start, 1000, context.nRuns, "ft_runtime", false)

      DumpTex(loggers, alg_names, start_def, 2000, context.nRuns, "der_runtime", true)
      DumpTex(loggers, alg_names, start, 2000, context.nRuns, "der_ft", true)

      DumpJson(loggers, alg_names, start_def, 2000, context.nRuns, "der_runtime", true)
      DumpJson(loggers, alg_names, start, 2000, context.nRuns, "der_ft", true)
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
    beta_from = args.getOption("--bfrom").toDouble,
    beta_to = args.getOption("--bto").toDouble,
    share    = args.getOption("--share").toDouble,
    nRuns    = args.getOption("--runs").toInt,
    nThreads = args.getOption("--threads").toInt
  )
}
