package ru.ifmo.onell.main

import java.io.PrintWriter
import java.nio.file.{Files, Paths}
import java.util.{Random, Arrays => JArrays}
import java.util.concurrent.ThreadLocalRandom

import scala.jdk.CollectionConverters._
import scala.util.Using

import ru.ifmo.onell.{HasIndividualOperations, Main, Optimizer}
import ru.ifmo.onell.algorithm.{OnePlusLambdaLambdaGA, OnePlusOneEA}
import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA._
import ru.ifmo.onell.algorithm.oll.CompatibilityLayer._
import ru.ifmo.onell.distribution.{BinomialDistribution, IntegerDistribution, PowerLawDistribution}
import ru.ifmo.onell.problem.mst.TreeOnlyMST
import ru.ifmo.onell.problem.mst.util.NaiveDynamicGraph
import ru.ifmo.onell.problem.{Jump, LinearRandomDoubleWeights, LinearRandomIntegerWeights, OneMax, OneMaxPerm, RandomPlanted3SAT, VertexCoverProblem}
import ru.ifmo.onell.util.par.{Executor, Multiplexer, ParallelExecutor, SequentialExecutor}

object RunningTimes extends Main.Module {
  override def name: String = "runtime"

  override def shortDescription: String = "Runs experiments on expected running times of algorithms on problems"

  override def longDescription: Seq[String] = Seq(
    "The following commands run experiments for problems on bit strings:",
    "  bits:om         <context>: for OneMax",
    "  bits:om:cap     <context>: same for heavy-tailed algorithms with various capping",
    "  bits:om:3d      <context>: same for the independent parameter sampling from three distributions",
    "  bits:om:sqrt    <context>: same but starting at the distance of sqrt(n) from the end",
    "  bits:om:log     <context>: same but starting at the distance of log(n+1) from the end",
    "  bits:om:lin     <context>: same but starting at the distance of d from the end, d is passed with --d option",
    "                             (several values may be passed comma-separated)",
    "  bits:sat        <context>: same for the MAX-SAT problem with logarithmic density",
    "  bits:sat:cap    <context>: same for heavy-tailed algorithms with various capping",
    "  bits:sat:sqrt   <context>: same but starting at the distance of sqrt(n) from the end",
    "  bits:sat:log    <context>: same but starting at the distance of log(n+1) from the end",
    "  bits:sat:lin    <context>: same but starting at the distance of d from the end, d is passed with --d option",
    "                             (several values may be passed comma-separated)",
    "  bits:om:tuning  <context>: for OneMax with various tuning choices for the (1+(λ,λ)) GA",
    "  bits:l2d:tuning <context>: same for linear functions with random weights from [1;2]",
    "  bits:l5d:tuning <context>: same for linear functions with random weights from [1;5]",
    "  bits:sat:tuning <context>: same for the MAX-SAT problem with logarithmic density",
    "  bits:om:tuning*  <context> <file1>,<file2>,...: for OneMax with various tuning choices",
    "                                                 for the (1+(λ,λ)) GA with constants tuned by irace",
    "  bits:l2d:tuning* <context> <file1>,<file2>,...: same for linear functions with random weights from [1;2]",
    "  bits:l5d:tuning* <context> <file1>,<file2>,...: same for linear functions with random weights from [1;5]",
    "  bits:sat:tuning* <context> <file1>,<file2>,...: same for the MAX-SAT problem with logarithmic density",
    "  bits:l2d:lambda <context>: experiments for lambda tunings for linear functions with random real-valued weights from [1;2]",
    "  bits:l5d:lambda <context>: same for linear functions with random real-valued weights from [1;5]",
    "  bits:om:lambda  <context>: same for OneMax",
    "  bits:l2i:lambda <context>: same for linear functions with random integer weights from [1;2]",
    "  bits:l5i:lambda <context>: same for linear functions with random integer weights from [1;5]",
    "  bits:lni:lambda <context>: same for linear functions with random integer weights from [1;n]",
    "  bits:sat:lambda <context>: same for the MAX-SAT problem with logarithmic density",
    "  bits:mst-tree   <context>: same for the minimum spanning tree problem with individuals encoding E=V-1 graphs",
    "  bits:vcp        <context>: same for the vertex cover problem (here, --from and --to control actual vertex numbers)",
    "  bits:jump:3d    <context>: same for Jump and independent parameter sampling from three distributions",
    "                             (--from and --to control problem size directly)",
    "The following commands run experiments for problems on permutations:",
    "  perm:om         <context>: for the permutation flavour of OneMax",
    "The <context> arguments, all mandatory, are:",
    "  --from     <int>: the minimum power of two for the problem size",
    "  --to       <int>: the maximum power of two for the problem size",
    "  --runs     <int>: the number of independent runs for each configuration",
    "  --threads  <int>: the number of threads to use (less than one: all available threads)",
    "  --out <filename>: the name of the JSON file to save the results"
  )

  override def moduleMain(args: Array[String]): Unit = args(0) match {
    case "bits:om"         => bitsOneMaxSimple(parseContext(args))
    case "bits:om:cap"     => bitsOneMaxCapping(parseContext(args))
    case "bits:om:3d"      => threeDistributionsOneMax(parseContext(args))
    case "bits:om:sqrt"    => bitsOneMaxAlmostOptimal(parseContext(args), n => Seq(math.sqrt(n).toInt))
    case "bits:om:log"     => bitsOneMaxAlmostOptimal(parseContext(args), n => Seq(math.log(n + 1).toInt))
    case "bits:l2d:lambda" => bitsParameterTuningLinearDouble(parseContext(args), 2.0)
    case "bits:l5d:lambda" => bitsParameterTuningLinearDouble(parseContext(args), 5.0)
    case "bits:om:lambda"  => bitsParameterTuningLinearInteger(parseContext(args), _ => 1)
    case "bits:l2i:lambda" => bitsParameterTuningLinearInteger(parseContext(args), _ => 2)
    case "bits:l5i:lambda" => bitsParameterTuningLinearInteger(parseContext(args), _ => 5)
    case "bits:lni:lambda" => bitsParameterTuningLinearInteger(parseContext(args), n => n)
    case "bits:sat:lambda" => bitsParameterTuningMaxSAT(parseContext(args))
    case "bits:sat"        => bitsMaxSATSimple(parseContext(args))
    case "bits:sat:cap"    => bitsMaxSATCapping(parseContext(args))
    case "bits:sat:sqrt"   => bitsMaxSATAlmostOptimal(parseContext(args), n => Seq(math.sqrt(n).toInt))
    case "bits:sat:log"    => bitsMaxSATAlmostOptimal(parseContext(args), n => Seq(math.log(n + 1).toInt))
    case "bits:vcp"        => vertexCoverPSSimple(parseContext(args))
    case "bits:mst-tree"   => treeOnlyMST(parseContext(args))
    case "bits:jump:3d"    => threeDistributionsJump(parseContext(args))
    case "perm:om"         => permOneMaxSimple(parseContext(args))
    case "bits:om:tuning"  => bitsOneMaxAllTuningChoices(parseContext(args))
    case "bits:l2d:tuning" => bitsLinearTunings(parseContext(args), 2.0)
    case "bits:l5d:tuning" => bitsLinearTunings(parseContext(args), 5.0)
    case "bits:sat:tuning" => bitsMaxSatTunings(parseContext(args))
    case "bits:om:tuning*" => bitsOneMaxIRacedTuningChoices(parseContext(args), args.getOption("--files"))
    case "bits:l2d:tuning*" => bitsLinearDoubleIRacedTuningChoices(parseContext(args), 2.0, args.getOption("--files"))
    case "bits:l5d:tuning*" => bitsLinearDoubleIRacedTuningChoices(parseContext(args), 5.0, args.getOption("--files"))
    case "bits:sat:tuning*" => bitsMaxSatIRacedTuningChoices(parseContext(args), args.getOption("--files"))
    case "bits:om:lin" =>
      val distances = args.getOption("--d").split(',').toIndexedSeq.map(_.toInt)
      bitsOneMaxAlmostOptimal(parseContext(args), _ => distances)
    case "bits:sat:lin" =>
      val distances = args.getOption("--d").split(',').toIndexedSeq.map(_.toInt)
      bitsMaxSATAlmostOptimal(parseContext(args), _ => distances)
  }

  private class Context(powers: Range, nRuns: Int, nThreads: Int, outName: String) {
    private[this] val jsonPrefix = "["
    private[this] val jsonSeparator = "\n,"
    private[this] val jsonSuffix = "\n]\n"

    def run(fun: (Executor[String], Int) => Any): Unit = {
      Using.resource(new PrintWriter(outName)) { moreOut =>
        Using.resource(makeScheduler(moreOut)) { scheduler =>
          val multiplexer = new Multiplexer(scheduler, nRuns)
          for (p <- powers) {
            fun(multiplexer, 1 << p)
          }
        }
      }
    }

    private def makeScheduler(moreOut: PrintWriter): Executor[String] = if (nThreads == 1) {
      new SequentialExecutor(moreOut, jsonPrefix, jsonSeparator, jsonSuffix)
    } else {
      new ParallelExecutor(moreOut, jsonPrefix, jsonSeparator, jsonSuffix, nThreads)
    }
  }

  private def bitsOneMaxSimple(context: Context): Unit = {
    val algorithms = Seq(
      "RLS" -> OnePlusOneEA.RLS,
      "(1+1) EA" -> OnePlusOneEA.Resampling,
      "(1+(λ,λ)) GA, λ<=n" -> createOnePlusLambdaLambdaGA(defaultOneFifthLambda, 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ<=2ln n" -> createOnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ~pow(2.1)" -> createOnePlusLambdaLambdaGA(powerLawLambda(2.1), 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ~pow(2.3)" -> createOnePlusLambdaLambdaGA(powerLawLambda(2.3), 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ~pow(2.5)" -> createOnePlusLambdaLambdaGA(powerLawLambda(2.5), 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ~pow(2.7)" -> createOnePlusLambdaLambdaGA(powerLawLambda(2.7), 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ~pow(2.9)" -> createOnePlusLambdaLambdaGA(powerLawLambda(2.9), 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ=6" -> createOnePlusLambdaLambdaGA(fixedLambda(6), 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ=8" -> createOnePlusLambdaLambdaGA(fixedLambda(8), 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ=10" -> createOnePlusLambdaLambdaGA(fixedLambda(10), 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ=12" -> createOnePlusLambdaLambdaGA(fixedLambda(12), 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ=fixed optimal" -> createOnePlusLambdaLambdaGA(fixedLogTowerLambda, 'R', "RL", 'C', 'D'),
    )

    context.run { (scheduler, n) =>
      for ((name, alg) <- algorithms) {
        scheduler addTask {
          val time = alg.optimize(new OneMax(n))
          s"""{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private def bitsOneMaxCapping(context: Context): Unit = {
    val algorithms = for {
      (beta, capLimit) <- Seq(2.1 -> 1024, 2.3 -> 4096, 2.5 -> 32768, 2.7 -> 32768, 2.9 -> 32768)
      cap <- (2 to 30).map(1 << _).filter(_ <= capLimit)
    } yield {
      (beta, cap, createOnePlusLambdaLambdaGA(powerLawLambda(beta, _ => cap), 'R', "RL", 'C', 'D'))
    }

    context.run { (scheduler, n) =>
      for ((beta, cap, alg) <- algorithms) {
        scheduler addTask {
          val t0 = System.nanoTime()
          val time = alg.optimize(new OneMax(n))
          val wcTime = (System.nanoTime() - t0) / 1e9
          s"""{"n":$n,"beta":"$beta","cap":"$cap","runtime":$time,"runtime over n":${time.toDouble / n},"wall-clock time":$wcTime}"""
        }
      }
    }
  }

  private def bitsOneMaxAlmostOptimal(context: Context, startValues: Int => Seq[Int]): Unit = {
    val algorithms = Seq(
      "RLS" -> OnePlusOneEA.RLS,
      "(1+1) EA" -> OnePlusOneEA.Resampling,
      "(1+(λ,λ)) GA, λ<=n" -> createOnePlusLambdaLambdaGA(defaultOneFifthLambda, 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ<=2ln n" -> createOnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ~pow(2.1)" -> createOnePlusLambdaLambdaGA(powerLawLambda(2.1), 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ~pow(2.3)" -> createOnePlusLambdaLambdaGA(powerLawLambda(2.3), 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ~pow(2.5)" -> createOnePlusLambdaLambdaGA(powerLawLambda(2.5), 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ~pow(2.7)" -> createOnePlusLambdaLambdaGA(powerLawLambda(2.7), 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ~pow(2.9)" -> createOnePlusLambdaLambdaGA(powerLawLambda(2.9), 'R', "RL", 'C', 'D'),
    )

    context.run { (scheduler, n) =>
      for ((name, alg) <- algorithms) {
        for (sv <- startValues(n)) {
          implicit val almostOptimalBitStringOps: HasIndividualOperations[Array[Boolean]] = new StartFromDistance(sv)
          scheduler addTask {
            val time = alg.optimize(new OneMax(n))(indOps = almostOptimalBitStringOps, deltaOps = implicitly)
            s"""{"n":$n,"algorithm":"$name","runtime":$time,"expected initial distance":$sv,"runtime over n":${time.toDouble / n}}"""
          }
        }
      }
    }
  }

  private val tuningChoices = {
    val lambdaStrategies = Seq("λ=8" -> fixedLambda(8) _,
                               "λ<=n" -> defaultOneFifthLambda _,
                               "λ<=log n" -> logCappedOneFifthLambda _)
    val mutationStrengths = Seq("standard" -> MutationStrength.Standard,
                                "shift" -> MutationStrength.Shift,
                                "resampling" -> MutationStrength.Resampling)
    val crossoverStrengths = Seq("standard on lambda" -> CrossoverStrength.StandardL,
                                 "standard on distance" -> CrossoverStrength.StandardD,
                                 "shift on lambda" -> CrossoverStrength.ShiftL,
                                 "shift on distance" -> CrossoverStrength.ShiftD,
                                 "resampling on lambda" -> CrossoverStrength.ResamplingL,
                                 "resampling on distance" -> CrossoverStrength.ResamplingD)
    val goodMutantStrategies = Seq("ignore" -> GoodMutantStrategy.Ignore,
                                   "skip crossover" -> GoodMutantStrategy.SkipCrossover,
                                   "do not count identical" -> GoodMutantStrategy.DoNotCountIdentical,
                                   "do not sample identical" -> GoodMutantStrategy.DoNotSampleIdentical)
    val populationSizeRoundings = Seq("round down" -> PopulationSizeRounding.AlwaysDown,
                                      "round up" -> PopulationSizeRounding.AlwaysUp,
                                      "probabilistic" -> PopulationSizeRounding.Probabilistic)
    for {
      (l, lambdaStrategy) <- lambdaStrategies
      (m, mutationStrength) <- mutationStrengths
      (c, crossoverStrength) <- crossoverStrengths
      (g, goodMutantStrategy) <- goodMutantStrategies
      (r, rounding) <- populationSizeRoundings
    } yield {
      val jsonNamePart = s""""lambda":"$l","mutation":"$m","crossover":"$c","good mutant":"$g","rounding":"$r""""
      val algGenerator = () => createOnePlusLambdaLambdaGA(lambdaStrategy, mutationStrength, crossoverStrength,
                                                         goodMutantStrategy, rounding)
      jsonNamePart -> algGenerator
    }
  }

  private def bitsOneMaxAllTuningChoices(context: Context): Unit = {
    context.run { (scheduler, n) =>
      for ((jsonName, algGenerator) <- tuningChoices) {
        scheduler addTask {
          val time = algGenerator().optimize(new OneMax(n))
          s"""{"n":$n,"irace":0,$jsonName,"runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private def bitsOneMaxIRacedTuningChoices(context: Context, fileList: String): Unit = {
    val allLines = fileList
      .split(',')
      .flatMap(filename => Files
        .readAllLines(Paths.get(filename))
        .asScala
        .filter(_.nonEmpty)
        .toIndexedSeq)
    context.run { (scheduler, n) =>
      for (line <- allLines) {
        scheduler addTask {
          val args = line.split(" ").filter(_.nonEmpty)
          val name = IRaceClient.parseOptimizerJson("oll", args)
          val algorithm = IRaceClient.parseOptimizer("oll", args)
          val time = algorithm.optimize(new OneMax(n))
          s"""{"n":$n,"irace":1,$name,"runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private val parameterTuningExperimentAlgorithmSelectionDouble = Seq(
    ("RLS", OnePlusOneEA.RLS),
    ("(1+1) EA", OnePlusOneEA.Standard),
    ("*(1+1) EA", OnePlusOneEA.Shift),
    ("$\\\\lambda=8$", createOnePlusLambdaLambdaGA(fixedLambda(8), 'S', "SL", 'I', 'U')),
    ("$\\\\lambdabound=n$", createOnePlusLambdaLambdaGA(defaultOneFifthLambda, 'S', "SL", 'I', 'U')),
    ("*$\\\\lambda=8$", createOnePlusLambdaLambdaGA(fixedLambda(8), 'H', "HD", 'C', 'U')),
    ("*$\\\\lambdabound=n$", createOnePlusLambdaLambdaGA(defaultOneFifthLambda, 'H', "HD", 'C', 'U')),
  )

  private val parameterTuningExperimentAlgorithmSelection = Seq(
    ("RLS", OnePlusOneEA.RLS),
    ("(1+1) EA", OnePlusOneEA.Standard),
    ("*(1+1) EA", OnePlusOneEA.Shift),
    ("$\\\\lambda=8$", createOnePlusLambdaLambdaGA(fixedLambda(8), 'S', "SL", 'I', 'P')),
    ("$\\\\lambdabound=n$", createOnePlusLambdaLambdaGA(defaultOneFifthLambda, 'S', "SL", 'I', 'P')),
    ("$\\\\lambdabound\\\\sim\\\\ln n$", createOnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'S', "SL", 'I', 'P')),
    ("*$\\\\lambda=8$", createOnePlusLambdaLambdaGA(fixedLambda(8), 'H', "HD", 'C', 'P')),
    ("*$\\\\lambdabound=n$", createOnePlusLambdaLambdaGA(defaultOneFifthLambda, 'H', "HD", 'C', 'P')),
    ("*$\\\\lambdabound\\\\sim\\\\ln n$", createOnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'H', "HD", 'C', 'P')),
  )

  private def bitsParameterTuningLinearDouble(context: Context, maxWeight: Double): Unit = {
    val seeder = new Random(314252354)
    context.run { (scheduler, n) =>
      for ((name, alg) <- parameterTuningExperimentAlgorithmSelectionDouble) {
        scheduler addTask {
          val t0 = System.nanoTime()
          val time = alg.optimize(new LinearRandomDoubleWeights(n, maxWeight, seeder.nextLong()))
          val wcTime = (System.nanoTime() - t0) * 1e-9
          s"""{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n":${time.toDouble / n},"wall-clock time":$wcTime}"""
        }
      }
    }
  }

  private def bitsParameterTuningLinearInteger(context: Context, maxWeight: Int => Int): Unit = {
    val seeder = new Random(314252354)
    context.run { (scheduler, n) =>
      for ((name, alg) <- parameterTuningExperimentAlgorithmSelection) {
        scheduler addTask {
          val t0 = System.nanoTime()
          val time = alg.optimize(new LinearRandomIntegerWeights(n, maxWeight(n), seeder.nextLong()))
          val wcTime = (System.nanoTime() - t0) * 1e-9
          s"""{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n":${time.toDouble / n},"wall-clock time":$wcTime}"""
        }
      }
    }
  }

  private def bitsParameterTuningMaxSAT(context: Context): Unit = {
    val seeder = new Random(314252354)
    context.run { (scheduler, n) =>
      val nClauses = (4 * n * math.log(n)).toInt
      for ((name, alg) <- parameterTuningExperimentAlgorithmSelection) {
        scheduler addTask {
          val t0 = System.nanoTime()
          val time = alg.optimize(new RandomPlanted3SAT(n, nClauses, RandomPlanted3SAT.EasyGenerator, seeder.nextLong()))
          val consumed = (System.nanoTime() - t0) * 1e-9
          s"""{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n":${time.toDouble / n},"wall-clock time":$consumed}"""
        }
      }
    }
  }

  private def bitsLinearTunings(context: Context, maxWeight: Double): Unit = {
    val seeder = new Random(314252354)
    context.run { (scheduler, n) =>
      for ((jsonName, algGenerator) <- tuningChoices) {
        scheduler addTask {
          val time = algGenerator().optimize(new LinearRandomDoubleWeights(n, maxWeight, seeder.nextLong()))
          s"""{"n":$n,"irace":0,$jsonName,"runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private def bitsLinearDoubleIRacedTuningChoices(context: Context, maxWeight: Double, fileList: String): Unit = {
    val seeder = new Random(314252354)
    val allLines = fileList
      .split(',')
      .flatMap(filename => Files
        .readAllLines(Paths.get(filename))
        .asScala
        .filter(_.nonEmpty)
        .toIndexedSeq)
    context.run { (scheduler, n) =>
      for (line <- allLines) {
        scheduler addTask {
          val args = line.split(" ").filter(_.nonEmpty)
          val name = IRaceClient.parseOptimizerJson("oll", args)
          val algorithm = IRaceClient.parseOptimizer("oll", args)
          val time = algorithm.optimize(new LinearRandomDoubleWeights(n, maxWeight, seeder.nextLong()))
          s"""{"n":$n,"irace":1,$name,"runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private def bitsMaxSATSimple(context: Context): Unit = {
    def makeHeavy(betaTen: Range, cap: Long => Long, capName: String): Seq[(String, Optimizer)] = betaTen map { b10 =>
      val b = b10 / 10.0
      val name = s"(1+(λ,λ)) GA, λ~pow($b), u=$capName"
      val algo = createOnePlusLambdaLambdaGA(powerLawLambda(b, cap), 'R', "RL", 'C', 'U')
      (name, algo)
    }

    val basicAlgorithms = Seq(
      "RLS" -> OnePlusOneEA.RLS,
      "(1+1) EA" -> OnePlusOneEA.Resampling,
      "(1+(λ,λ)) GA, λ<=n" -> createOnePlusLambdaLambdaGA(defaultOneFifthLambda, 'R', "RL", 'C', 'U'),
      "(1+(λ,λ)) GA, λ<=2ln(n+1)" -> createOnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'R', "RL", 'C', 'U'),
    )

    val algorithms = basicAlgorithms ++
      makeHeavy(21 to 29 by 2, n => n / 2, "n/2") ++
      makeHeavy(21 to 29 by 2, n => math.ceil(math.sqrt(n.toDouble)).toLong, "sqrt(n)") ++
      makeHeavy(11 to 29 by 2, n => math.ceil(2 * math.log(n + 1.0)).toLong, "2ln(n+1)")

    val seeder = new Random(314252354)
    context.run { (scheduler, n) =>
      val nClauses = (4 * n * math.log(n)).toInt
      for ((name, alg) <- algorithms) {
        scheduler addTask {
          val t0 = System.nanoTime()
          val time = alg.optimize(new RandomPlanted3SAT(n, nClauses, RandomPlanted3SAT.EasyGenerator, seeder.nextLong()))
          val consumed = (System.nanoTime() - t0) * 1e-9
          s"""{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n":${time.toDouble / n},"wall-clock time":$consumed}"""
        }
      }
    }
  }

  private def bitsMaxSATCapping(context: Context): Unit = {
    def makeHeavy(b: Double, cap: Int): Optimizer = {
      createOnePlusLambdaLambdaGA(powerLawLambda(b, _ => cap), 'R', "RL", 'C', 'U')
    }

    val algorithms = for {
      (beta, capLimit) <- Seq(2.1 -> 256, 2.3 -> 32768, 2.5 -> 32768, 2.7 -> 32768, 2.9 -> 32768)
      cap <- (2 to 13).map(1 << _).filter(_ <= capLimit)
    } yield {
      (beta, cap, makeHeavy(beta, cap))
    }

    val seeder = new Random(314252354)
    context.run { (scheduler, n) =>
      val nClauses = (4 * n * math.log(n)).toInt
      for ((beta, cap, alg) <- algorithms) {
        scheduler addTask {
          val t0 = System.nanoTime()
          val time = alg.optimize(new RandomPlanted3SAT(n, nClauses, RandomPlanted3SAT.EasyGenerator, seeder.nextLong()))
          val wcTime = (System.nanoTime() - t0) / 1e9
          s"""{"n":$n,"beta":"$beta","cap":"$cap","runtime":$time,"runtime over n":${time.toDouble / n},"wall-clock time":$wcTime}"""
        }
      }
    }
  }

  private def bitsMaxSatTunings(context: Context): Unit = {
    val seeder = new Random(314252354)
    context.run { (scheduler, n) =>
      val nClauses = (4 * n * math.log(n)).toInt
      for ((jsonName, algGenerator) <- tuningChoices) {
        scheduler addTask {
          val time = algGenerator().optimize(new RandomPlanted3SAT(n, nClauses, RandomPlanted3SAT.EasyGenerator, seeder.nextLong()))
          s"""{"n":$n,"irace":0,$jsonName,"runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private def bitsMaxSatIRacedTuningChoices(context: Context, fileList: String): Unit = {
    val seeder = new Random(314252354)
    val allLines = fileList
      .split(',')
      .flatMap(filename => Files
        .readAllLines(Paths.get(filename))
        .asScala
        .filter(_.nonEmpty)
        .toIndexedSeq)
    context.run { (scheduler, n) =>
      val nClauses = (4 * n * math.log(n)).toInt
      for (line <- allLines) {
        scheduler addTask {
          val args = line.split(" ").filter(_.nonEmpty)
          val name = IRaceClient.parseOptimizerJson("oll", args)
          val algorithm = IRaceClient.parseOptimizer("oll", args)
          val time = algorithm.optimize(new RandomPlanted3SAT(n, nClauses, RandomPlanted3SAT.EasyGenerator, seeder.nextLong()))
          s"""{"n":$n,"irace":1,$name,"runtime":$time,"runtime over n":${time.toDouble / n}}"""
        }
      }
    }
  }

  private def bitsMaxSATAlmostOptimal(context: Context, startValues: Int => Seq[Int]): Unit = {
    val algorithms = Seq(
      ("RLS", Int.MaxValue, OnePlusOneEA.RLS),
      ("(1+1) EA", Int.MaxValue, OnePlusOneEA.Resampling),
      ("(1+(λ,λ)) GA, λ<=n", 16384, createOnePlusLambdaLambdaGA(defaultOneFifthLambda, 'R', "RL", 'C', 'U')),
      ("(1+(λ,λ)) GA, λ<=2ln n", Int.MaxValue, createOnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'R', "RL", 'C', 'U')),
      ("(1+(λ,λ)) GA, λ~pow(2.1)", Int.MaxValue, createOnePlusLambdaLambdaGA(powerLawLambda(2.1), 'R', "RL", 'C', 'U')),
      ("(1+(λ,λ)) GA, λ~pow(2.3)", Int.MaxValue, createOnePlusLambdaLambdaGA(powerLawLambda(2.3), 'R', "RL", 'C', 'U')),
      ("(1+(λ,λ)) GA, λ~pow(2.5)", Int.MaxValue, createOnePlusLambdaLambdaGA(powerLawLambda(2.5), 'R', "RL", 'C', 'U')),
      ("(1+(λ,λ)) GA, λ~pow(2.7)", Int.MaxValue, createOnePlusLambdaLambdaGA(powerLawLambda(2.7), 'R', "RL", 'C', 'U')),
      ("(1+(λ,λ)) GA, λ~pow(2.9)", Int.MaxValue, createOnePlusLambdaLambdaGA(powerLawLambda(2.9), 'R', "RL", 'C', 'U')),
    )

    val seeder = new Random(314252354)
    context.run { (scheduler, n) =>
      for ((name, limit, alg) <- algorithms) {
        if (n <= limit) {
          for (sv <- startValues(n)) {
            implicit val almostOptimalBitStringOps: HasIndividualOperations[Array[Boolean]] = new StartFromDistance(sv)
            scheduler addTask {
              val time = alg.optimize(new RandomPlanted3SAT(n, (4 * n * math.log(n)).toInt,
                                                            RandomPlanted3SAT.EasyGenerator, seeder.nextLong()))
              s"""{"n":$n,"algorithm":"$name","runtime":$time,"initial distance":$sv,"runtime over n":${time.toDouble / n}}"""
            }
          }
        }
      }
    }
  }

  private def permOneMaxSimple(context: Context): Unit = {
    val algorithms = Seq(
      ("RLS", Int.MaxValue, OnePlusOneEA.RLS),
      ("(1+1) EA", Int.MaxValue, OnePlusOneEA.Resampling),
      ("(1+(λ,λ)) GA, λ=10", Int.MaxValue, createOnePlusLambdaLambdaGA(fixedLambda(10), 'R', "RL", 'C', 'U')),
      ("(1+(λ,λ)) GA, λ=2ln n", Int.MaxValue, createOnePlusLambdaLambdaGA(fixedLogLambda, 'R', "RL", 'C', 'U')),
      ("(1+(λ,λ)) GA, λ<=2ln n", Int.MaxValue, createOnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'R', "RL", 'C', 'U')),
      ("(1+(λ,λ)) GA, λ<=n", 256, createOnePlusLambdaLambdaGA(defaultOneFifthLambda, 'R', "RL", 'C', 'U')),
      ("(1+(λ,λ)) GA, λ~pow(2.5)", 4096, createOnePlusLambdaLambdaGA(powerLawLambda(2.5), 'R', "RL", 'C', 'U')),
    )

    context.run { (scheduler, n) =>
      for ((name, maxN, alg) <- algorithms) {
        if (n <= maxN) {
          scheduler.addTask {
            val time = alg.optimize(new OneMaxPerm(n))
            s"""{"n":$n,"algorithm":"$name","runtime":$time,"runtime over n2":${time.toDouble / n / n}}"""
          }
        }
      }
    }
  }

  private def vertexCoverPSSimple(context: Context): Unit = {
    val algorithms = Seq(
      "(1+1) EA" -> OnePlusOneEA.Resampling,
      "(1+(λ,λ)) GA, λ<=2ln n" -> createOnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ~pow(2.5)" -> createOnePlusLambdaLambdaGA(powerLawLambda(2.5), 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ=10" -> createOnePlusLambdaLambdaGA(fixedLambda(10), 'R', "RL", 'C', 'D'),
    )

    context.run { (scheduler, nn) =>
      val k = Integer.numberOfTrailingZeros(nn)
      val vcp = VertexCoverProblem.makePSProblem(k)
      for ((name, alg) <- algorithms) {
        scheduler addTask {
          implicit val individualOps: HasIndividualOperations[VertexCoverProblem.Individual] = vcp
          val t0 = System.nanoTime()
          val time = alg.optimize(vcp)
          val consumed = (System.nanoTime() - t0) * 1e-9
          s"""{"n":${vcp.nVertices},"algorithm":"$name","runtime":$time,"wall-clock time":$consumed}"""
        }
      }
    }
  }

  private def treeOnlyMST(context: Context): Unit = {
    val algorithms = Seq(
      "(1+1) EA" -> OnePlusOneEA.Resampling,
      "(1+(λ,λ)) GA, λ<=2ln n" -> createOnePlusLambdaLambdaGA(logCappedOneFifthLambda, 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ~pow(2.5)" -> createOnePlusLambdaLambdaGA(powerLawLambda(2.5, i => math.sqrt(i.toDouble).toLong), 'R', "RL", 'C', 'D'),
      "(1+(λ,λ)) GA, λ=10" -> createOnePlusLambdaLambdaGA(fixedLambda(10), 'R', "RL", 'C', 'D'),
    )

    val rng = new java.util.Random(872454326413212L)
    context.run { (scheduler, n) =>
      val e = 2 * n
      val mst = TreeOnlyMST.randomGraph(n, e, 1, 2, rng, NaiveDynamicGraph)
      for ((name, alg) <- algorithms) {
        scheduler addTask {
          implicit val individualOps: HasIndividualOperations[TreeOnlyMST.Individual] = mst
          val t0 = System.nanoTime()
          val time = alg.optimize(mst)
          val consumed = (System.nanoTime() - t0) * 1e-9
          s"""{"nVertices":$n,"nEdges":$e,"algorithm":"$name","runtime":$time,"wall-clock time":$consumed}"""
        }
      }
    }
  }

  private def threeDistributionsOneMax(context: Context): Unit = {
    val algorithms = for {
      betaL <- Seq(2.0, 2.2, 2.4, 2.6, 2.8, 3.0, 3.2)
      distL = PowerLawDistribution(1L << 27, betaL) // with betaL=2, this takes 96M values and several gigs of memory
      betaP <- Seq(1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2)
      betaC <- Seq(1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2)
    } yield {
      def distLFun(n: Long): IntegerDistribution = if (n < 27) PowerLawDistribution(1L << n, betaL) else distL
      val controller = new ThreeDistributionController(betaP, betaC, distLFun)
      (betaP, betaC, betaL, new OnePlusLambdaLambdaGA(controller, BehaviorForGoodMutant.IgnoreExistence, CompatibilityOptions(true)))
    }

    context.run { (scheduler, n) =>
      for ((betaP, betaC, betaL, alg) <- algorithms) {
        scheduler.addTask {
          val time = alg.optimize(new OneMax(n))
          s"""{"n":$n,"betaP":$betaP,"betaC":$betaC,"betaL":$betaL,"runtime":$time}"""
        }
      }
    }
  }

  private def threeDistributionsJump(context: Context): Unit = {
    val algorithms = for {
      betaL <- Seq(2.0, 2.2, 2.4)
      distL = PowerLawDistribution(1L << 27, betaL) // with betaL=2, this takes 96M values and few gigs of memory
      betaP <- Seq(1.0, 1.2, 1.4)
      betaC <- Seq(1.0, 1.2, 1.4)
    } yield {
      def distLFun(n: Long): IntegerDistribution = if (n < 27) PowerLawDistribution(1L << n, betaL) else distL
      val controller = new ThreeDistributionController(betaP, betaC, distLFun)
      (betaP, betaC, betaL, new OnePlusLambdaLambdaGA(controller, BehaviorForGoodMutant.IgnoreExistence, CompatibilityOptions(true)))
    }

    context.run { (scheduler, n) =>
      for ((betaP, betaC, betaL, alg) <- algorithms) {
        for (k <- 2 to 6 if k * 4 <= n) {
          scheduler.addTask {
            val time = alg.optimize(new Jump(n, k))
            s"""{"n":$n,"betaL":$betaL,"betaP":$betaP,"betaC":$betaC,"k":$k,"runtime":$time}"""
          }
        }
      }
    }
  }

  private class ThreeDistributionController(betaP: Double, betaC: Double, distL: Long => IntegerDistribution)
    extends ParameterControllerCreator {
    override def apply(nChanges: Long): ParameterController = new ParameterController {
      private val nSqrt = math.sqrt(nChanges.toDouble)
      private val powerLawP = PowerLawDistribution(nSqrt.toInt, betaP)
      private val powerLawC = PowerLawDistribution(nSqrt.toInt, betaC)
      private val powerLawLambda = distL(nChanges)

      override def getParameters(rng: ThreadLocalRandom): IterationParameters = {
        val lambda = powerLawLambda.sample(rng)
        val mutantDistance = BinomialDistribution.standard(nChanges, powerLawP.sample(rng) / nSqrt).sample(rng)
        IterationParameters(
          firstPopulationSize = lambda,
          secondPopulationSize = lambda,
          numberOfChangesInEachMutant = mutantDistance,
          numberOfChangesInCrossoverOffspring =
            BinomialDistribution.standard(mutantDistance, powerLawC.sample(rng) / nSqrt)
        )
      }

      override def receiveFeedback(budgetSpent: Long, childToParentComparison: Int): Unit = {}
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

  private class StartFromDistance(start: Int) extends HasIndividualOperations[Array[Boolean]] {
    override def createStorage(problemSize: Int): Array[Boolean] = new Array(problemSize)
    override def initializeRandomly(individual: Array[Boolean], rng: ThreadLocalRandom): Unit = {
      var remainingPoints = start
      val len = individual.length
      JArrays.fill(individual, true)
      while (remainingPoints > 0) {
        val idx = rng.nextInt(len)
        if (individual(idx)) {
          individual(idx) = false
          remainingPoints -= 1
        }
      }
    }
  }

  private def parseContext(args: Array[String]): Context = new Context(
    powers   = args.getOption("--from").toInt to args.getOption("--to").toInt,
    nRuns    = args.getOption("--runs").toInt,
    nThreads = args.getOption("--threads").toInt,
    outName  = args.getOption("--out")
  )
}
