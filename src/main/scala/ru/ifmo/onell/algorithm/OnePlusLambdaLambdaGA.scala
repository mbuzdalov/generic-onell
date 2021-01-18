package ru.ifmo.onell.algorithm

import java.util.concurrent.{ThreadLocalRandom => Random}

import scala.annotation.tailrec
import scala.{specialized => sp}

import ru.ifmo.onell._
import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA._
import ru.ifmo.onell.distribution.IntegerDistribution
import ru.ifmo.onell.util.OrderedSet
import ru.ifmo.onell.util.Specialization.{changeSpecialization => csp, fitnessSpecialization => fsp}

class OnePlusLambdaLambdaGA(parameterControllerCreator: ParameterControllerCreator,
                            behaviorForGoodMutant: BehaviorForGoodMutant,
                            compatibilityOptions: CompatibilityOptions = CompatibilityOptions.Default)
  extends Optimizer
{
  override def optimize[I, @sp(fsp) F, @sp(csp) C]
    (fitness: Fitness[I, F, C],
     iterationLogger: IterationLogger[F])
    (implicit deltaOps: HasDeltaOperations[C], indOps: HasIndividualOperations[I]): Long =
  {
    val problemSize = fitness.problemSize
    val nChanges = fitness.numberOfChanges
    val nChangesL = fitness.changeIndexTypeToLong(nChanges)
    val paramController = parameterControllerCreator(nChangesL)
    val rng = Random.current()
    val individual = indOps.createStorage(problemSize)
    val mutation, mutationBest, crossover, crossoverBest = deltaOps.createStorage(nChanges)
    val aux = new Aux[F]

    @tailrec
    def runMutationsEtc(remaining: Int, baseFitness: F, change: Int, bestFitness: F): F = {
      if (remaining == 0) {
        bestFitness
      } else {
        deltaOps.initializeDelta(mutation, nChanges, change, rng)
        val currFitness = fitness.evaluateAssumingDelta(individual, mutation, baseFitness)
        if (fitness.compare(bestFitness, currFitness) < 0) {
          mutationBest.copyFrom(mutation)
          runMutationsEtc(remaining - 1, baseFitness, change, currFitness)
        } else {
          runMutationsEtc(remaining - 1, baseFitness, change, bestFitness)
        }
      }
    }

    def runMutations(remaining: Int, baseFitness: F, change: Int): F = {
      deltaOps.initializeDelta(mutation, nChanges, change, rng)
      mutationBest.copyFrom(mutation)
      val currentFitness = fitness.evaluateAssumingDelta(individual, mutation, baseFitness)
      runMutationsEtc(remaining - 1, baseFitness, change, currentFitness)
    }

    def updateOnParent(result: Aux[F], currFitness: F, isFirstTime: Boolean): Unit =
      if (isFirstTime || fitness.compare(currFitness, result.fitness) > 0) {
        crossoverBest.clear()
        result.fitness = currFitness
      }

    def updateOnCrossover(result: Aux[F], currFitness: F, source: OrderedSet[C], isFirstTime: Boolean): Unit =
      if (isFirstTime || fitness.compare(currFitness, result.fitness) > 0) {
        crossoverBest.copyFrom(source)
        result.fitness = currFitness
      }

    def runCrossover(remaining: Int, baseFitness: F, mutantFitness: F, mutantDistance: Int,
                     distribution: IntegerDistribution, result: Aux[F]): Int = {
      var triedQueries, testedQueries = 0
      while (triedQueries < remaining) {
        triedQueries += 1
        val distance = distribution.sample(rng)
        if (distance == 0) {
          updateOnParent(result, baseFitness, testedQueries == 0)
          testedQueries += 1
        } else if (distance == mutantDistance) {
          updateOnCrossover(result, mutantFitness, mutationBest, testedQueries == 0)
          if (compatibilityOptions.countCrossoverOffspringIdenticalToBestMutantWhenDifferentFromParent)
            testedQueries += 1
        } else {
          deltaOps.initializeDeltaFromExisting(crossover, mutationBest, distance, rng)
          val newFitness = fitness.evaluateAssumingDelta(individual, crossover, baseFitness)
          updateOnCrossover(result, newFitness, crossover, testedQueries == 0)
          testedQueries += 1
        }
      }
      testedQueries
    }

    @tailrec
    def iteration(f: F, evaluationsSoFar: Long): Long = if (fitness.isOptimalFitness(f)) evaluationsSoFar else {
      val IterationParameters(mutationPopSize, crossoverPopSize, mutantDistance, crossDistribution) =
        paramController.getParameters(rng)

      var newEvaluations = evaluationsSoFar
      val bestChildFitness = if (mutantDistance == 0) {
        // Always simulate mutations, but skip crossovers if they don't support zero distance
        val crossoverContribution = if (crossDistribution.isEmpty) 0 else crossoverPopSize
        newEvaluations += mutationPopSize + crossoverContribution
        crossoverBest.clear()
        f
      } else {
        val bestMutantFitness = runMutations(mutationPopSize, f, mutantDistance)
        val isMutantBetter = fitness.compare(bestMutantFitness, f) > 0
        if (crossDistribution.isEmpty || behaviorForGoodMutant == BehaviorForGoodMutant.SkipCrossover && isMutantBetter) {
          // Crossovers are impossible (first case) or unwanted (second case) on this iteration, we just stop the phase there
          newEvaluations += mutationPopSize
          if (isMutantBetter) {
            crossoverBest.copyFrom(mutationBest)
            bestMutantFitness
          } else {
            crossoverBest.clear()
            f
          }
        } else {
          val crossEvs = runCrossover(crossoverPopSize, f, bestMutantFitness, mutantDistance, crossDistribution, aux)
          if (behaviorForGoodMutant == BehaviorForGoodMutant.UpdateParent) {
            if (fitness.compare(bestMutantFitness, aux.fitness) > 0) {
              aux.fitness = bestMutantFitness
              crossoverBest.copyFrom(mutationBest)
            }
          }

          newEvaluations += mutationPopSize + crossEvs
          aux.fitness
        }
      }

      val budgetSpent = newEvaluations - evaluationsSoFar
      val fitnessComparison = fitness.compare(bestChildFitness, f)
      paramController.receiveFeedback(budgetSpent, fitnessComparison)

      val nextFitness = if (fitnessComparison >= 0) {
        val theFitness = fitness.applyDelta(individual, crossoverBest, f)
        assert(fitness.compare(bestChildFitness, theFitness) == 0,
               s"Fitness incremental evaluation seems broken: $bestChildFitness != $theFitness")
        theFitness
      } else f
      iterationLogger.logIteration(newEvaluations, bestChildFitness)
      iteration(nextFitness, newEvaluations)
    }

    indOps.initializeRandomly(individual, rng)
    val firstFitness = fitness.evaluate(individual)
    iterationLogger.logIteration(1, firstFitness)
    iteration(firstFitness, 1)
  }
}

object OnePlusLambdaLambdaGA {
  case class IterationParameters(firstPopulationSize: Int,
                                 secondPopulationSize: Int,
                                 numberOfChangesInEachMutant: Int,
                                 numberOfChangesInCrossoverOffspring: IntegerDistribution)

  trait ParameterController {
    def getParameters(rng: Random): IterationParameters
    def receiveFeedback(budgetSpent: Long, childToParentComparison: Int): Unit
  }

  trait ParameterControllerCreator {
    def apply(nChanges: Long): ParameterController
  }

  sealed trait BehaviorForGoodMutant
  object BehaviorForGoodMutant {
    case object IgnoreExistence extends BehaviorForGoodMutant
    case object SkipCrossover extends BehaviorForGoodMutant
    case object UpdateParent extends BehaviorForGoodMutant
  }

  case class CompatibilityOptions(countCrossoverOffspringIdenticalToBestMutantWhenDifferentFromParent: Boolean)
  object CompatibilityOptions {
    final val Default = CompatibilityOptions(countCrossoverOffspringIdenticalToBestMutantWhenDifferentFromParent = false)
  }

  private final class Aux[@sp(fsp) F] {
    var fitness: F = _
  }
}
