package ru.ifmo.onell.main

import ru.ifmo.onell.*
import ru.ifmo.onell.algorithm.oll.CompatibilityLayer.*
import ru.ifmo.onell.problem.{LinearRandomDoubleWeights, OneMax, RandomPlanted3SAT}

import java.net.{DatagramPacket, DatagramSocket}
import scala.compiletime.uninitialized

object IRaceClient extends Main.Module:
  override def name: String = "irace"
  override def shortDescription: String = "Runs computations for irace"
  override def longDescription: Seq[String] = Seq(
    "Executes the given algorithm for the given problem, as configured by the irace parameter tuning program.",
    "The command-line parameters have the following syntax:",
    "  --run-as-server <port1>,<port2>,...: listens on UDP sockets on given ports for packets formatted as below",
    "  --algorithm <algorithm> --problem <problem> <parameters>: directly run the algorithm on the problem,",
    "                                                            which can also be sent through a socket",
    "",
    "The parameters may be related to either the algorithm or the problem.",
    "Common parameters:",
    "  --max-evaluations <long>: the limit on the number of evaluations",
    "  --mode evaluations|fitness|minus-fitness: tell what to report, the number of evaluations, or the best fitness",
    "",
    "Supported algorithms:",
    "  oll: the (1+(λ,λ)) genetic algorithm. Supported parameters:",
    "    --lambda-tuning  linear|log|fixed: the lambda tuning strategy to use",
    "    --lambda         <double>: the value for lambda to use when --lambda-tuning is 'fixed'",
    "    --tuning-success <double>: the multiple to apply to lambda if fitness is increased",
    "    --tuning-failure <double>: the multiple to apply to lambda if fitness is not increased",
    "    --mutation-strength-alg   standard|shift|resampling: the algorithm to sample mutation strength",
    "    --crossover-strength-alg  standard|shift|resampling: the algorithm to sample crossover strength",
    "    --crossover-strength-base lambda|distance: the denominator for the probability to sample crossover strength",
    "    --good-mutant-strategy    ignore|skip-crossover|do-not-count|do-not-sample: the strategy to treat",
    "                              the mutants which are good enough to compete with parents",
    "    --mutation-q   <double>: the additional multiple to the standard mutation probability",
    "    --crossover-q  <double>: the additional multiple to the standard crossover probability",
    "    --crossover-popsize-q <double>: the additional multiple to the standard crossover population size",
    "    --popsize-rounding:   round-up|round-down|probabilistic: the strategy to compute integer population sizes",
    "",
    "Supported problems:",
    "  OneMax: the famous OneMax function. Supported parameters:",
    "    --n <int>: the problem size",
    "  Linear: the randomly generated linear pseudo-Boolean function. Supported parameters:",
    "    --n          <int>: the problem size",
    "    --seed       <long>: the random seed to use",
    "    --max-weight <double>: the maximum weight to sample (the minimum is always 1.0)",
    "  MaxSat: the satisfiable randomly generated maximum 3CNF-SAT problem. Supported parameters:",
    "    --n         <int>: the number of Boolean variables",
    "    --clauses   <int>: the number of clauses",
    "    --seed     <long>: the random seed to use",
    "    --generator easy|hard: the clause generator to use",
  )

  override def moduleMain(args: Array[String]): Unit =
    if args.contains("--run-as-server") then
      val ports = args.getOption("--run-as-server").split(',').map(_.toInt)
      for port <- ports do
        val thread = Thread(() => runServer(port))
        thread.setName(s"worker-$port")
        println(s"Starting thread ${thread.getName} to listen on port $port")
        thread.start()
    else println(runMany(args))

  private def runServer(port: Int): Unit =
    val socket = DatagramSocket(port)
    val array = Array.ofDim[Byte](10240)
    val packet = DatagramPacket(array, array.length)
    try receiveCommandsUntilTheEnd(socket, array, packet) finally socket.close()

  @scala.annotation.tailrec
  private def receiveCommandsUntilTheEnd(socket: DatagramSocket, theArray: Array[Byte], packet: DatagramPacket): Unit =
    packet.setData(theArray)
    socket.setSendBufferSize(10240)
    socket.setReceiveBufferSize(10240)
    socket.receive(packet)
    val text = String(packet.getData, packet.getOffset, packet.getLength)
    val result = runMany(text.split(" "))
    val resultBytes = result.getBytes
    packet.setData(resultBytes)
    socket.send(packet)
    receiveCommandsUntilTheEnd(socket, theArray, packet)

  private def runMany(args: Array[String]): String = try
    val nRuns = args.getOption("--average-over").toInt
    var sum = 0.0
    var idx = 0
    while idx < nRuns && !sum.isInfinite do
      sum += runOne(args)
      idx += 1
    (sum / nRuns).toString
  catch
    case e: Throwable => e.toString

  private def runOne(command: Array[String]): Double =
    val optimizer = parseOptimizer(command.getOption("--algorithm"), command)
    val maxEvaluations = command.getOption("--max-evaluations").toLong
    val mode = command.getOption("--mode") match
      case "evaluations" => ReportEvaluationsUntilOptimum
      case "fitness" => ReportBestFitnessUnderLimits
      case "minus-fitness" => ReportMinusFitnessUnderLimits
    command.getOption("--problem") match 
      case "OneMax" => runUntilOptimum(optimizer, maxEvaluations, mode,
                                       OneMax(command.getOption("--n").toInt))
      case "Linear" => runUntilOptimum(optimizer, maxEvaluations, mode,
                                       LinearRandomDoubleWeights(command.getOption("--n").toInt,
                                                                     command.getOption("--max-weight").toDouble,
                                                                     command.getOption("--seed").toLong))
      case "MaxSat" => runUntilOptimum(optimizer, maxEvaluations, mode,
                                       RandomPlanted3SAT(command.getOption("--n").toInt,
                                                         command.getOption("--clauses").toInt,
                                                         command.getOption("--generator") match
                                                           case "easy" => RandomPlanted3SAT.EasyGenerator
                                                           case "hard" => RandomPlanted3SAT.HardGenerator,
                                                         command.getOption("--seed").toLong))
      case _ => throw IllegalArgumentException(s"Unknown problem name '${command(1)}'. Supported names: OneMax, Linear, MaxSat'")

  private case class ParameterAndJson[T](value: T, json: String)

  private def parseLambdaTuning(args: Array[String]): ParameterAndJson[Long => LambdaTuning] =
    args.getOption("--lambda-tuning") match
      case "linear" => ParameterAndJson(oneFifthLambda(
        onSuccess = args.getOption("--tuning-success").toDouble,
        onFailure = args.getOption("--tuning-failure").toDouble,
        threshold = _.toDouble
      ), """"lambda":"λ<=n"""")
      case "log" => ParameterAndJson(oneFifthLambda(
        onSuccess = args.getOption("--tuning-success").toDouble,
        onFailure = args.getOption("--tuning-failure").toDouble,
        threshold = size => 2 * math.log(size + 1.0)
      ), """"lambda":"λ<=log n"""")
      case "fixed" =>
        val value = args.getOption("--lambda").toInt
        ParameterAndJson(fixedLambda(value), s""""lambda":"λ=$value"""")

  private def parseMutationStrength(args: Array[String]): ParameterAndJson[MutationStrength] =
    args.getOption("--mutation-strength-alg") match
      case "standard"   => ParameterAndJson(MutationStrength.Standard, """"mutation":"standard"""")
      case "shift"      => ParameterAndJson(MutationStrength.Shift, """"mutation":"shift"""")
      case "resampling" => ParameterAndJson(MutationStrength.Resampling, """"mutation":"resampling"""")

  private def parseCrossoverStrength(args: Array[String]): ParameterAndJson[CrossoverStrength] =
    args.getOption("--crossover-strength-alg") match
      case "standard" => args.getOption("--crossover-strength-base") match
        case "lambda"   => ParameterAndJson(CrossoverStrength.StandardL, """"crossover":"standard on lambda"""")
        case "distance" => ParameterAndJson(CrossoverStrength.StandardD, """"crossover":"standard on distance"""")
      case "shift" => args.getOption("--crossover-strength-base") match
        case "lambda"   => ParameterAndJson(CrossoverStrength.ShiftL, """"crossover":"shift on lambda"""")
        case "distance" => ParameterAndJson(CrossoverStrength.ShiftD, """"crossover":"shift on distance"""")
      case "resampling" => args.getOption("--crossover-strength-base") match
        case "lambda"   => ParameterAndJson(CrossoverStrength.ResamplingL, """"crossover":"resampling on lambda"""")
        case "distance" => ParameterAndJson(CrossoverStrength.ResamplingD, """"crossover":"resampling on distance"""")

  private def parseGoodMutantStrategy(args: Array[String]): ParameterAndJson[GoodMutantStrategy] =
    args.getOption("--good-mutant-strategy") match
      case "ignore" => ParameterAndJson(GoodMutantStrategy.Ignore, """"good mutant":"ignore"""")
      case "skip-crossover" => ParameterAndJson(GoodMutantStrategy.SkipCrossover, """"good mutant":"skip crossover"""")
      case "do-not-count" => ParameterAndJson(GoodMutantStrategy.DoNotCountIdentical, """"good mutant":"do not count identical"""")
      case "do-not-sample" => ParameterAndJson(GoodMutantStrategy.DoNotSampleIdentical, """"good mutant":"do not sample identical"""")

  private def parseRounding(args: Array[String]): ParameterAndJson[PopulationSizeRounding] =
    args.getOption("--popsize-rounding") match
      case "round-up" => ParameterAndJson(PopulationSizeRounding.AlwaysUp, """"rounding":"round up"""")
      case "round-down" => ParameterAndJson(PopulationSizeRounding.AlwaysDown, """"rounding":"round down"""")
      case "probabilistic" => ParameterAndJson(PopulationSizeRounding.Probabilistic, """"rounding":"probabilistic"""")

  def parseOptimizerJson(optimizerName: String, args: Array[String]): String =
    optimizerName match
      case "oll" =>
        s"""${
          parseLambdaTuning(args).json
        },${
          parseMutationStrength(args).json
        },${
          parseCrossoverStrength(args).json
        },${
          parseGoodMutantStrategy(args).json
        },${
          parseRounding(args).json
        }"""
      case _ => throw IllegalArgumentException(s"Unknown optimizer name '$optimizerName'. Supported names: oll")

  def parseOptimizer(optimizerName: String, args: Array[String]): Optimizer =
    optimizerName match
      case "oll" => createOnePlusLambdaLambdaGA(
        lambdaTuning = parseLambdaTuning(args).value,
        mutationStrength = parseMutationStrength(args).value,
        crossoverStrength = parseCrossoverStrength(args).value,
        goodMutantStrategy = parseGoodMutantStrategy(args).value,
        constantTuning = ConstantTuning(
          mutationProbabilityQuotient = args.getOption("--mutation-q").toDouble,
          crossoverProbabilityQuotient = args.getOption("--crossover-q").toDouble,
          crossoverPopulationSizeQuotient = args.getOption("--crossover-popsize-q").toDouble
        ),
        populationRounding = parseRounding(args).value
      )
      case _ => throw IllegalArgumentException(s"Unknown optimizer name '$optimizerName'. Supported names: oll")

  private def runUntilOptimum[I, F: Numeric, C]
                             (optimizer: Optimizer, maxEvaluations: Long, reportMode: ReportMode, problem: Fitness[I, F, C])
                             (using HasDeltaOperations[C], HasIndividualOperations[I]): Double =
    import scala.math.Numeric.Implicits.given
    val logger = SimpleTerminationLogger[F](maxEvaluations, (l, r) => problem.compare(l, r) < 0)
    try
      val normalizedIterations = optimizer.optimize(problem, logger).toDouble / problem.problemSize
      reportMode match
        case ReportEvaluationsUntilOptimum => normalizedIterations
        case ReportBestFitnessUnderLimits => logger.getBestFitness.toDouble
        case ReportMinusFitnessUnderLimits => -logger.getBestFitness.toDouble
    catch
      case EvaluationExceededException => reportMode match
        case ReportEvaluationsUntilOptimum => Double.PositiveInfinity
        case ReportBestFitnessUnderLimits => logger.getBestFitness.toDouble
        case ReportMinusFitnessUnderLimits => -logger.getBestFitness.toDouble

  extension (args: Array[String])
    private def getOption(option: String): String =
      val index = args.indexOf(option)
      if index < 0 then throw IllegalArgumentException(s"No option '$option' is given")
      if index + 1 == args.length then throw IllegalArgumentException(s"Option '$option' should have an argument")
      args(index + 1)

  private class SimpleTerminationLogger[F](maxEvaluations: Long, isLess: (F, F) => Boolean) extends IterationLogger[F]:
    private var bestFitness: F = uninitialized
    private var bestFitnessInitialized: Boolean = false
    override def logIteration(evaluations: Long, fitness: F): Unit =
      if !bestFitnessInitialized || isLess(fitness, bestFitness) then
        bestFitness = fitness
        bestFitnessInitialized = true
      if evaluations > maxEvaluations then throw EvaluationExceededException
    def getBestFitness: F = bestFitness

  private object EvaluationExceededException extends RuntimeException

  private sealed trait ReportMode
  private case object ReportEvaluationsUntilOptimum extends ReportMode
  private case object ReportBestFitnessUnderLimits extends ReportMode
  private case object ReportMinusFitnessUnderLimits extends ReportMode
