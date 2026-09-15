package ru.ifmo.onell

import ru.ifmo.onell.individual.{BitStringOps, PermutationOps}
import ru.ifmo.onell.util.Permutation

import java.util.random.RandomGenerator

/**
  * This trait encapsulates the knowledge about basic operations with individuals.
  *
  * @tparam MutableIndividual the type of individual.
  */
trait HasIndividualOperations[MutableIndividual]:
  /**
    * Creates a storage for an individual, given the problem size.
    * @param problemSize the problem size.
    * @return the newly created individual.
    */
  def createStorage(problemSize: Int): MutableIndividual

  /**
    * Initializes an individual randomly using the provided random number generator.
    * @param individual the individual to be initialized.
    * @param rng the random number generator.
    */
  def initializeRandomly(individual: MutableIndividual, rng: RandomGenerator): Unit


/**
  * This companion object contains several known implementations of the `HasIndividualOperations` trait.
  */
object HasIndividualOperations:
  implicit def forBooleanArray: HasIndividualOperations[Array[Boolean]] = BitStringOps
  implicit def forPermutation: HasIndividualOperations[Permutation] = PermutationOps
