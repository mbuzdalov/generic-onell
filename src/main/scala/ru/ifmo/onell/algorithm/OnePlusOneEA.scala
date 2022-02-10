package ru.ifmo.onell.algorithm

import java.util.concurrent.{ThreadLocalRandom => Random}

import scala.annotation.tailrec
import scala.{specialized => sp}

import ru.ifmo.onell.util.Specialization.{changeSpecialization => csp, fitnessSpecialization => fsp}
import ru.ifmo.onell._
import ru.ifmo.onell.distribution.{BinomialDistribution, IntegerDistribution, PowerLawDistribution}

/**
  * This is the companion class to `OnePlusOneEA` containing several variations of the (1+1) EA
  * with most popular mutations, including RLS.
  */
object OnePlusOneEA {
  /**
    * This is randomized local search, whose mutation always flips one randomly chosen bit.
    */
  final val RLS = new OnePlusOneEA(_ => 1)

  /**
    * This is the standard (1+1) EA, which flips every bit independently with probability 1/n.
    */
  final val Standard = new OnePlusOneEA(n => BinomialDistribution.standard(n, 1.0 / n))

  /**
    * This is the (1+1) EA which flips every bit independently with probability 1/n,
    * but if zero bits were flipped, it flips one randomly chosen bit.
    *
    * From the literature, this flavour is known as "shift mutation".
    */
  final val Shift = new OnePlusOneEA(n => BinomialDistribution.shift(n, 1.0 / n))

  /**
    * This is the (1+1) EA which flips every bit independently with probability 1/n,
    * but if zero bits were flipped, it continues sampling.
    *
    * From the literature, this flavour is known as "resampling mutation".
    */
  final val Resampling = new OnePlusOneEA(n => BinomialDistribution.resampling(n, 1.0 / n))

  /**
    * This is the (1+1) EA which flips the number of bits sampled from the power-law distribution
    * with the given parameter beta.
    *
    * This algorithm is different from the "classic" fast (1+1) EA in that it does not delegate
    * the bit sampling work to the binomial distribution with the first parameter sampled from the binomial one.
    * What is more, the raw power-law distribution never returns 0, so we don't need to shift or resample
    * zero-bit flips in order to be efficient.
    *
    * @param beta the parameter to control the power-law distribution. Good to be in (1;3).
    * @return the (1+1) EA directly using the power-law distribution with the given beta.
    */
  def heavyDirect(beta: Double) = new OnePlusOneEA(n => PowerLawDistribution(n, beta))
}

/**
  * This is the (1+1) EA parameterized with a distribution on the number of bits to be flipped.
  */
class OnePlusOneEA(distributionGenerator: Long => IntegerDistribution) extends Optimizer {
  final def optimize[I, @sp(fsp) F, @sp(csp) C]
    (fitness: Fitness[I, F, C], iterationLogger: IterationLogger[F])
    (implicit deltaOps: HasDeltaOperations[C], indOps: HasIndividualOperations[I]): Long =
  {
    val problemSize = fitness.problemSize
    val nChanges = fitness.numberOfChanges
    val individual = indOps.createStorage(problemSize)
    val delta = deltaOps.createStorage(nChanges)
    val rng = Random.current()

    val mutationDistribution = distributionGenerator(fitness.changeIndexTypeToLong(nChanges))

    @tailrec
    def iterate(f: F, soFar: Long): Long = if (fitness.isOptimalFitness(f)) soFar else {
      val sz = mutationDistribution.sample(rng)
      if (sz == 0) {
        iterationLogger.logIteration(soFar + 1, f)
        iterate(f, soFar + 1)
      } else {
        deltaOps.initializeDelta(delta, nChanges, sz, rng)
        val newF = fitness.applyDelta(individual, delta, f)
        val comparison = fitness.compare(f, newF)
        iterationLogger.logIteration(soFar + 1, newF)
        if (comparison <= 0) {
          iterate(newF, soFar + 1)
        } else {
          fitness.unapplyDelta(individual, delta)
          iterate(f, soFar + 1)
        }
      }
    }

    indOps.initializeRandomly(individual, rng)
    val firstFitness = fitness.evaluate(individual)
    iterationLogger.logIteration(1, firstFitness)
    iterate(firstFitness, 1)
  }
}
