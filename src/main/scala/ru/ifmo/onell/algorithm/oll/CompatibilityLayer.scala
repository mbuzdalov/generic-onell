package ru.ifmo.onell.algorithm.oll

import java.util.concurrent.{ThreadLocalRandom => Random}

import scala.annotation.tailrec
import scala.language.implicitConversions

import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA
import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA._
import ru.ifmo.onell.distribution.{BinomialDistribution, IntegerDistribution, PowerLawDistribution}
import ru.ifmo.onell.distribution.BinomialDistribution._

object CompatibilityLayer {
  def createOnePlusLambdaLambdaGA(lambdaTuning: Long => LambdaTuning,
                                  mutationStrength: MutationStrength,
                                  crossoverStrength: CrossoverStrength,
                                  goodMutantStrategy: GoodMutantStrategy,
                                  populationRounding: PopulationSizeRounding,
                                  constantTuning: ConstantTuning = defaultTuning) =
    new OnePlusLambdaLambdaGA(
      parameterControllerCreator = createParameterController(lambdaTuning, mutationStrength, crossoverStrength,
                                                             goodMutantStrategy, populationRounding, constantTuning),
      behaviorForGoodMutant = createBehaviorForGoodMutant(goodMutantStrategy),
      compatibilityOptions = createCompatibilityOptions(goodMutantStrategy)
    )

  private def createParameterController(lambdaTuning: Long => LambdaTuning,
                                mutationStrength: MutationStrength,
                                crossoverStrength: CrossoverStrength,
                                goodMutantStrategy: GoodMutantStrategy,
                                populationRounding: PopulationSizeRounding,
                                constantTuning: ConstantTuning = defaultTuning): ParameterControllerCreator =
    (nChanges: Long) => new ParameterController {
      private[this] val tuning = lambdaTuning(nChanges)

      override def getParameters(rng: Random): IterationParameters = {
        val lambda = tuning.lambda(rng)
        val mutationPopSize = math.max(1, populationRounding(lambda, rng))
        val crossoverPopSize = math.max(1, populationRounding(lambda * constantTuning.crossoverPopulationSizeQuotient, rng))

        val mutantDistance = mutationStrength(nChanges, constantTuning.mutationProbabilityQuotient * lambda).sample(rng)
        val q = constantTuning.crossoverProbabilityQuotient
        val crossDistribution = {
          val crossDistribution0 = crossoverStrength(lambda, mutantDistance, q)
          if (mutantDistance == 0 || goodMutantStrategy != GoodMutantStrategy.DoNotSampleIdentical)
            crossDistribution0
          else if (crossDistribution0.minValue == mutantDistance)
            IntegerDistribution.empty
          else new ResampleUntilLess(crossDistribution0, mutantDistance)
        }

        IterationParameters(
          firstPopulationSize = mutationPopSize,
          secondPopulationSize = crossoverPopSize,
          numberOfChangesInEachMutant = mutantDistance,
          numberOfChangesInCrossoverOffspring = crossDistribution
        )
      }

      override def receiveFeedback(budgetSpent: Long, childToParentComparison: Int): Unit =
        if (childToParentComparison < 0)
          tuning.notifyChildIsWorse(budgetSpent)
        else if (childToParentComparison == 0)
          tuning.notifyChildIsEqual(budgetSpent)
        else
          tuning.notifyChildIsBetter(budgetSpent)
    }

  private def createBehaviorForGoodMutant(goodMutantStrategy: GoodMutantStrategy): BehaviorForGoodMutant =
    goodMutantStrategy match {
      case GoodMutantStrategy.Ignore => BehaviorForGoodMutant.IgnoreExistence
      case GoodMutantStrategy.SkipCrossover => BehaviorForGoodMutant.SkipCrossover
      case GoodMutantStrategy.DoNotCountIdentical => BehaviorForGoodMutant.UpdateParent
      case GoodMutantStrategy.DoNotSampleIdentical => BehaviorForGoodMutant.UpdateParent
    }

  private def createCompatibilityOptions(goodMutantStrategy: GoodMutantStrategy): CompatibilityOptions =
    goodMutantStrategy match {
      case GoodMutantStrategy.Ignore => CompatibilityOptions(countCrossoverOffspringIdenticalToBestMutantWhenDifferentFromParent = true)
      case GoodMutantStrategy.SkipCrossover => CompatibilityOptions(countCrossoverOffspringIdenticalToBestMutantWhenDifferentFromParent = true)
      case _ => CompatibilityOptions.Default
    }


  private[this] val probEps = 1e-10

  trait PopulationSizeRounding {
    def apply(fpValue: Double, rng: Random): Int
  }

  object PopulationSizeRounding {
    final val AlwaysUp: PopulationSizeRounding = (fpValue: Double, _: Random) => math.ceil(fpValue).toInt
    final val AlwaysDown: PopulationSizeRounding = (fpValue: Double, _: Random) => fpValue.toInt
    final val Probabilistic: PopulationSizeRounding = (fpValue: Double, rng: Random) => {
      val lower = math.floor(fpValue).toInt
      val upper = math.ceil(fpValue).toInt
      if (lower == upper || rng.nextDouble() < upper - fpValue) lower else upper
    }

    implicit def u2alwaysUp(dummy: 'U'): PopulationSizeRounding = AlwaysUp
    implicit def d2alwaysDown(dummy: 'D'): PopulationSizeRounding = AlwaysDown
    implicit def p2probabilistic(dummy: 'P'): PopulationSizeRounding = Probabilistic
  }

  trait MutationStrength {
    def apply(nChanges: Long, multipliedLambda: Double): IntegerDistribution
  }

  object MutationStrength {
    final val Standard: MutationStrength = (n, l) => BinomialDistribution.standard(n, l / n)
    final val Resampling: MutationStrength = (n, l) => if (l < probEps) 1 else BinomialDistribution.resampling(n, l / n)
    final val Shift: MutationStrength = (n, l) => BinomialDistribution.shift(n, l / n)

    implicit def s2standard(dummy: 'S'): MutationStrength = Standard
    implicit def r2resampling(dummy: 'R'): MutationStrength = Resampling
    implicit def h2shift(dummy: 'H'): MutationStrength = Shift
  }

  trait CrossoverStrength {
    def apply(lambda: Double, mutantDistance: Int, quotient: Double): IntegerDistribution
  }

  object CrossoverStrength {
    import math.max

    private def standardize(p: Double): Double = if (p >= 1 - probEps) 1 else p

    final val StandardL:   CrossoverStrength = (l, d, q) => standard(d, standardize(q / l))
    final val StandardD:   CrossoverStrength = (_, d, q) => standard(d, standardize(q / max(d, 1)))
    final val ResamplingL: CrossoverStrength = (l, d, q) => resampling(d, standardize(q / l))
    final val ResamplingD: CrossoverStrength = (_, d, q) => resampling(d, standardize(q / max(d, 1)))
    final val ShiftL:      CrossoverStrength = (l, d, q) => shift(d, standardize(q / l))
    final val ShiftD:      CrossoverStrength = (_, d, q) => shift(d, standardize(q / max(d, 1)))

    implicit def sl2standardL(dummy: "SL"): CrossoverStrength = StandardL
    implicit def sd2standardD(dummy: "SD"): CrossoverStrength = StandardD
    implicit def rl2resamplingL(dummy: "RL"): CrossoverStrength = ResamplingL
    implicit def rd2resamplingD(dummy: "RD"): CrossoverStrength = ResamplingD
    implicit def hl2shiftL(dummy: "HL"): CrossoverStrength = ShiftL
    implicit def hd2shiftD(dummy: "HD"): CrossoverStrength = ShiftD
  }

  sealed abstract class GoodMutantStrategy private (val incrementForTriedQueries: Int, val incrementForTestedQueries: Int)
  object GoodMutantStrategy {
    case object Ignore extends GoodMutantStrategy(1, 1)
    case object SkipCrossover extends GoodMutantStrategy(1, 1)
    case object DoNotCountIdentical extends GoodMutantStrategy(1, 0)
    case object DoNotSampleIdentical extends GoodMutantStrategy(0, 0)

    implicit def i2ignore(dummy: 'I'): Ignore.type = Ignore
    implicit def s2skip(dummy: 'S'): SkipCrossover.type = SkipCrossover
    implicit def c2doNotCount(dummy: 'C'): DoNotCountIdentical.type = DoNotCountIdentical
    implicit def m2doNotSample(dummy: 'M'): DoNotSampleIdentical.type = DoNotSampleIdentical
  }

  trait LambdaTuning {
    def lambda(rng: Random): Double
    def notifyChildIsBetter(budgetSpent: Long): Unit
    def notifyChildIsEqual(budgetSpent: Long): Unit
    def notifyChildIsWorse(budgetSpent: Long): Unit
  }

  //noinspection ScalaUnusedSymbol
  def fixedLambda(value: Double)(size: Long): LambdaTuning = new LambdaTuning {
    override def lambda(rng: Random): Double = value
    override def notifyChildIsBetter(budgetSpent: Long): Unit = {}
    override def notifyChildIsEqual(budgetSpent: Long): Unit = {}
    override def notifyChildIsWorse(budgetSpent: Long): Unit = {}
  }

  def fixedLogLambda(size: Long): LambdaTuning = new LambdaTuning {
    private[this] val theLambda = 2 * math.log(size + 1.0)
    override def lambda(rng: Random): Double = theLambda
    override def notifyChildIsBetter(budgetSpent: Long): Unit = {}
    override def notifyChildIsEqual(budgetSpent: Long): Unit = {}
    override def notifyChildIsWorse(budgetSpent: Long): Unit = {}
  }

  def fixedLogTowerLambda(size: Long): LambdaTuning = new LambdaTuning {
    private val logN = math.log(size + 1.0)
    private val logLogN = math.log(logN + 1.0)
    private val theLambda = math.sqrt(logN * logLogN / math.log(logLogN + 1.0)) * 2
    override def lambda(rng: Random): Double = theLambda
    override def notifyChildIsBetter(budgetSpent: Long): Unit = {}
    override def notifyChildIsEqual(budgetSpent: Long): Unit = {}
    override def notifyChildIsWorse(budgetSpent: Long): Unit = {}
  }

  def powerLawLambda(beta: Double)(size: Long): LambdaTuning = powerLawLambda(beta, n => n)(size)
  def powerLawLambda(beta: Double, limit: Long => Long)(size: Long): LambdaTuning = new LambdaTuning {
    private[this] val dist = PowerLawDistribution(limit(size), beta)
    override def lambda(rng: Random): Double = dist.sample(rng)
    override def notifyChildIsBetter(budgetSpent: Long): Unit = {}
    override def notifyChildIsEqual(budgetSpent: Long): Unit = {}
    override def notifyChildIsWorse(budgetSpent: Long): Unit = {}
  }

  def oneFifthLambda(onSuccess: Double, onFailure: Double, threshold: Long => Double)(size: Long): LambdaTuning = new LambdaTuning {
    private[this] var value = 1.0
    private[this] val maxValue = threshold(size)

    override def lambda(rng: Random): Double = value
    override def notifyChildIsBetter(budgetSpent: Long): Unit = value = math.min(maxValue, math.max(1, value * onSuccess))
    override def notifyChildIsEqual(budgetSpent: Long): Unit = notifyChildIsWorse(budgetSpent)
    override def notifyChildIsWorse(budgetSpent: Long): Unit = value = math.min(maxValue, math.max(1, value * onFailure))
  }

  def defaultOneFifthLambda(size: Long): LambdaTuning = oneFifthLambda(OneFifthOnSuccess, OneFifthOnFailure, _.toDouble)(size)
  def logCappedOneFifthLambda(size: Long): LambdaTuning = oneFifthLambda(OneFifthOnSuccess, OneFifthOnFailure, n => 2 * math.log(n + 1.0))(size)

  case class ConstantTuning(mutationProbabilityQuotient: Double,
                            crossoverProbabilityQuotient: Double,
                            crossoverPopulationSizeQuotient: Double)

  final val defaultTuning = ConstantTuning(1.0, 1.0, 1.0)
  final val OneFifthOnSuccess = 1 / 1.5
  final val OneFifthOnFailure = math.pow(1.5, 0.25)

  private[this] class ResampleUntilLess(base: IntegerDistribution, threshold: Int) extends IntegerDistribution {
    @tailrec
    override final def sample(rng: java.util.Random): Int = {
      val value = base.sample(rng)
      if (value < threshold) value else sample(rng)
    }
    override def minValue: Int = base.minValue
    override def maxValue: Int = math.min(base.maxValue, threshold - 1)
  }
}
