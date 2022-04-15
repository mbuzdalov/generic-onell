package ru.ifmo.onell.main.util

import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA
import ru.ifmo.onell.algorithm.oll.CompatibilityLayer._

object AlgorithmCodeNames {
  private val mutationDistributions = Map(
    'S' -> MutationStrength.Standard,
    'H' -> MutationStrength.Shift,
    'R' -> MutationStrength.Resampling,
  )

  private def expandMutationDistribution(char: Char): Seq[Char] = char match {
    case 'S' => "S"
    case 'H' => "H"
    case 'R' => "R"
    case '*' => "SHR"
    case c => throw new IllegalArgumentException(s"Illegal mutation distribution (first symbol) '$c', can be one of 'S', 'H', 'R', '*'")
  }

  private def expandCrossoverDistributionShape(char: Char): Seq[Char] = char match {
    case 'S' => "S"
    case 'H' => "H"
    case 'R' => "R"
    case '*' => "SHR"
    case c => throw new IllegalArgumentException(s"Illegal crossover distribution shape (second symbol) '$c', can be one of 'S', 'H', 'R', '*'")
  }

  private def expandCrossoverDistributionSource(char: Char): Seq[Char] = char match {
    case 'L' => "L"
    case 'D' => "D"
    case '*' => "LD"
    case c => throw new IllegalArgumentException(s"Illegal crossover distribution source (third symbol) '$c', can be one of 'L', 'D', '*'")
  }

  private def parseCrossoverDistribution(shape: Char, source: Char): CrossoverStrength =  s"$shape$source" match {
    case "SL" => CrossoverStrength.StandardL
    case "SD" => CrossoverStrength.StandardD
    case "HL" => CrossoverStrength.ShiftL
    case "HD" => CrossoverStrength.ShiftD
    case "RL" => CrossoverStrength.ResamplingL
    case "RD" => CrossoverStrength.ResamplingD
    case _ => throw new AssertionError()
  }

  private val goodMutantStrategies = Map(
    'I' -> GoodMutantStrategy.Ignore,
    'S' -> GoodMutantStrategy.SkipCrossover,
    'C' -> GoodMutantStrategy.DoNotCountIdentical,
    'M' -> GoodMutantStrategy.DoNotSampleIdentical,
  )

  private def expandGoodMutantStrategy(c: Char): Seq[Char] = c match {
    case 'I' => "I"
    case 'S' => "S"
    case 'C' => "C"
    case 'M' => "M"
    case '*' => "ISCM"
    case c =>
      throw new IllegalArgumentException(s"Illegal good mutant strategy (fourth symbol) '$c', can be one of 'I', 'S', 'C', 'M', '*'")
  }

  private val populationSizeRoundings = Map(
    'U' -> PopulationSizeRounding.AlwaysUp,
    'D' -> PopulationSizeRounding.AlwaysDown,
    'P' -> PopulationSizeRounding.Probabilistic,
  )

  private def expandPopulationSizeRounding(rnd: Char): Seq[Char] = rnd match {
    case 'U' => "U"
    case 'D' => "D"
    case 'P' => "P"
    case '*' => "UDP"
    case c =>
      throw new IllegalArgumentException(s"Illegal population size rounding (fifth symbol) '$c', can be one of 'U', 'D', 'P','*'")
  }

  def parseOnePlusLambdaLambdaGenerators(mask: String): Seq[
    ((Long => LambdaTuning) => OnePlusLambdaLambdaGA, String)
  ] = {
    for {
      singleMask <- mask.split(',').toIndexedSeq
      mutDistSym <- expandMutationDistribution(singleMask(0))
      crossDistShapeSym <- expandCrossoverDistributionShape(singleMask(1))
      crossDistSourceSym <- expandCrossoverDistributionSource(singleMask(2))
      goodMutantSym <- expandGoodMutantStrategy(singleMask(3))
      roundingSym <- expandPopulationSizeRounding(singleMask(4))
    } yield {
      val alg = (lambdaTuning: Long => LambdaTuning) => createOnePlusLambdaLambdaGA(
        lambdaTuning = lambdaTuning,
        mutationStrength = mutationDistributions(mutDistSym),
        crossoverStrength = parseCrossoverDistribution(crossDistShapeSym, crossDistSourceSym),
        goodMutantStrategy = goodMutantStrategies(goodMutantSym),
        constantTuning = defaultTuning,
        populationRounding = populationSizeRoundings(roundingSym)
      )
      val code = s"$mutDistSym$crossDistShapeSym$crossDistSourceSym$goodMutantSym$roundingSym"
      (alg, code)
    }
  }

  def parserDescriptionForOnePlusLambdaLambdaGenerators(paramName: String): Seq[String] = Seq(
    s"The $paramName parameters are one or more comma-separated five-character strings",
    "with the following meaning:",
    "  abcde",
    "  |||||",
    "  ||||+- population size rounding: up (U), down (D), probabilistic (P)",
    "  |||+-- good mutant strategy: ignore (I), skip crossover (S), do not count (C), do not sample (M)",
    "  ||+--- crossover distribution source: lambda (L), offspring distance (D)",
    "  |+---- crossover distribution shape: standard (S), shift (H), resampling (R)",
    "  +----- mutation distribution shape:  standard (S), shift (H), resampling (R)",
    "Each of these symbols can be '*' which means that each of the choices is tested.",
  )
}
