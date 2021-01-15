package ru.ifmo.onell.algorithm

import java.util.concurrent.{ThreadLocalRandom => Random}

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.{specialized => sp}

import ru.ifmo.onell._
import ru.ifmo.onell.algorithm.OnePlusLambdaLambdaGA._
import ru.ifmo.onell.distribution.{BinomialDistribution, IntegerDistribution, PowerLawDistribution}
import ru.ifmo.onell.util.OrderedSet
import ru.ifmo.onell.util.Specialization.{changeSpecialization => csp, fitnessSpecialization => fsp}

class OnePlusLambdaLambdaGA(lambdaTuning: Long => LambdaTuning,
                            mutationStrength: MutationStrength,
                            crossoverStrength: CrossoverStrength,
                            goodMutantStrategy: GoodMutantStrategy,
                            populationRounding: PopulationSizeRounding,
                            constantTuning: ConstantTuning = defaultTuning)
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
    val lambdaP = lambdaTuning(nChangesL)
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
        val distance = distribution.sample(rng)
        if (distance == 0) {
          updateOnParent(result, baseFitness, testedQueries == 0)
          testedQueries += 1
          triedQueries += 1
        } else if (distance == mutantDistance) {
          updateOnCrossover(result, mutantFitness, mutationBest, testedQueries == 0)
          triedQueries += goodMutantStrategy.incrementForTriedQueries
          testedQueries += goodMutantStrategy.incrementForTestedQueries
        } else {
          deltaOps.initializeDeltaFromExisting(crossover, mutationBest, distance, rng)
          val newFitness = fitness.evaluateAssumingDelta(individual, crossover, baseFitness)
          updateOnCrossover(result, newFitness, crossover, testedQueries == 0)
          triedQueries += 1
          testedQueries += 1
        }
      }
      testedQueries
    }

    @tailrec
    def iteration(f: F, evaluationsSoFar: Long): Long = if (fitness.isOptimalFitness(f)) evaluationsSoFar else {
      val lambda = lambdaP.lambda(rng)

      val mutationPopSize = math.max(1, populationRounding(lambda, rng))
      val crossoverPopSize = math.max(1, populationRounding(lambda * constantTuning.crossoverPopulationSizeQuotient, rng))

      val mutantDistance = mutationStrength(nChangesL, constantTuning.mutationProbabilityQuotient * lambda).sample(rng)
      val q = constantTuning.crossoverProbabilityQuotient
      val crossDistribution = crossoverStrength(lambda, mutantDistance, q)

      var newEvaluations = evaluationsSoFar
      val bestChildFitness = if (mutantDistance == 0) {
        // Always simulate mutations, but skip crossovers if they don't support zero distance
        val crossoverContribution = if (crossDistribution.isEmpty) 0 else crossoverPopSize
        newEvaluations += mutationPopSize + crossoverContribution
        crossoverBest.clear()
        f
      } else {
        val bestMutantFitness = runMutations(mutationPopSize, f, mutantDistance)
        if (goodMutantStrategy == GoodMutantStrategy.SkipCrossover && fitness.compare(bestMutantFitness, f) > 0) {
          newEvaluations += mutationPopSize
          crossoverBest.copyFrom(mutationBest)
          bestMutantFitness
        } else if (crossDistribution.minValue == mutantDistance && goodMutantStrategy == GoodMutantStrategy.DoNotSampleIdentical) {
          // A very special case, which would enter an infinite loop if not taken care.
          // With GoodMutantStrategy.DoNotSampleIdentical,
          // a crossover which would always sample a maximum number of bits to flip would cause an infinite loop.
          // For this reason we say specially that we don't try crossovers in this case.
          newEvaluations += mutationPopSize
          if (fitness.compare(bestMutantFitness, f) > 0) {
            crossoverBest.copyFrom(mutationBest)
            bestMutantFitness
          } else {
            crossoverBest.clear()
            f
          }
        } else {
          val crossEvs = runCrossover(crossoverPopSize, f, bestMutantFitness, mutantDistance, crossDistribution, aux)
          if (goodMutantStrategy == GoodMutantStrategy.DoNotSampleIdentical || goodMutantStrategy == GoodMutantStrategy.DoNotCountIdentical) {
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
      val fitnessComparison = fitness.compare(f, bestChildFitness)
      if (fitnessComparison < 0) {
        lambdaP.notifyChildIsBetter(budgetSpent)
      } else if (fitnessComparison > 0) {
        lambdaP.notifyChildIsWorse(budgetSpent)
      } else {
        lambdaP.notifyChildIsEqual(budgetSpent)
      }

      val nextFitness = if (fitnessComparison <= 0) {
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
    import BinomialDistribution._
    import math.{min, max}
    final val StandardL:   CrossoverStrength = (l, d, q) => standard(d, min(q / l, 1))
    final val StandardD:   CrossoverStrength = (_, d, q) => standard(d, min(q / max(d, 1), 1))
    final val ResamplingL: CrossoverStrength = (l, d, q) => resampling(d, min(q / l, 1))
    final val ResamplingD: CrossoverStrength = (_, d, q) => resampling(d, min(q / max(d, 1), 1))
    final val ShiftL:      CrossoverStrength = (l, d, q) => shift(d, min(q / l, 1))
    final val ShiftD:      CrossoverStrength = (_, d, q) => shift(d, min(q / max(d, 1), 1))

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

  def modifiedOneFifthLambda(onSuccess: Double, onFailure: Double, threshold: Long => Double)(size: Long): LambdaTuning = new LambdaTuning {
    private[this] var value, baseValue = 1.0
    private[this] val maxValue = threshold(size)
    private[this] var continuousFailedIterations, delta = 0L

    override def lambda(rng: Random): Double = value
    override def notifyChildIsBetter(budgetSpent: Long): Unit = {
      continuousFailedIterations = 0
      delta = 10
      value = math.min(maxValue, math.max(1, value * onSuccess))
      baseValue = value
    }
    override def notifyChildIsEqual(budgetSpent: Long): Unit = notifyChildIsWorse(budgetSpent)
    override def notifyChildIsWorse(budgetSpent: Long): Unit = {
      continuousFailedIterations += 1
      if (continuousFailedIterations == delta) {
        delta += 1
        continuousFailedIterations = 0
      }
      value = math.min(maxValue, math.max(1, baseValue * math.pow(onFailure, continuousFailedIterations.toDouble)))
    }
  }

  def modifiedOneFifthLambda(size: Long): LambdaTuning = modifiedOneFifthLambda(OneFifthOnSuccess, OneFifthOnFailure, _.toDouble)(size)
  def logCappedModifiedOneFifthLambda(size: Long): LambdaTuning = modifiedOneFifthLambda(OneFifthOnSuccess, OneFifthOnFailure, n => 2 * math.log(n + 1.0))(size)

  case class ConstantTuning(mutationProbabilityQuotient: Double,
                            crossoverProbabilityQuotient: Double,
                            crossoverPopulationSizeQuotient: Double)

  final val defaultTuning = ConstantTuning(1.0, 1.0, 1.0)
  final val OneFifthOnSuccess = 1 / 1.5
  final val OneFifthOnFailure = math.pow(1.5, 0.25)

  private final class Aux[@sp(fsp) F] {
    var fitness: F = _
  }
}
