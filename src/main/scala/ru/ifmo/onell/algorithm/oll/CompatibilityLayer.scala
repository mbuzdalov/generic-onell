package ru.ifmo.onell.algorithm.oll

import java.util.Random
import java.util.concurrent.ThreadLocalRandom

import scala.annotation.tailrec

import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA._
import ru.ifmo.onell.distribution.IntegerDistribution

object CompatibilityLayer {
  def createParameterController(lambdaTuning: Long => LambdaTuning,
                                mutationStrength: MutationStrength,
                                crossoverStrength: CrossoverStrength,
                                goodMutantStrategy: GoodMutantStrategy,
                                populationRounding: PopulationSizeRounding,
                                constantTuning: ConstantTuning = defaultTuning): ParameterControllerCreator =
    (nChanges: Long) => new ParameterController {
      private[this] val tuning = lambdaTuning(nChanges)

      override def getParameters(rng: ThreadLocalRandom): IterationParameters = {
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

  def createBehaviorForGoodMutant(goodMutantStrategy: GoodMutantStrategy): BehaviorForGoodMutant =
    goodMutantStrategy match {
      case GoodMutantStrategy.Ignore => BehaviorForGoodMutant.IgnoreExistence
      case GoodMutantStrategy.SkipCrossover => BehaviorForGoodMutant.SkipCrossover
      case GoodMutantStrategy.DoNotCountIdentical => BehaviorForGoodMutant.UpdateParent
      case GoodMutantStrategy.DoNotSampleIdentical => BehaviorForGoodMutant.UpdateParent
    }

  def createCompatibilityOptions(goodMutantStrategy: GoodMutantStrategy): CompatibilityOptions =
    goodMutantStrategy match {
      case GoodMutantStrategy.Ignore => CompatibilityOptions(countCrossoverOffspringIdenticalToBestMutantWhenDifferentFromParent = true)
      case GoodMutantStrategy.SkipCrossover => CompatibilityOptions(countCrossoverOffspringIdenticalToBestMutantWhenDifferentFromParent = true)
      case _ => CompatibilityOptions.Default
    }

  private[this] class ResampleUntilLess(base: IntegerDistribution, threshold: Int) extends IntegerDistribution {
    @tailrec
    override final def sample(rng: Random): Int = {
      val value = base.sample(rng)
      if (value < threshold) value else sample(rng)
    }
    override def minValue: Int = base.minValue
    override def maxValue: Int = math.min(base.maxValue, threshold - 1)
  }
}
