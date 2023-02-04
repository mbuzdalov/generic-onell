package ru.ifmo.onell

import java.util.concurrent.{ThreadLocalRandom => Random}

import ru.ifmo.onell.individual.{BitStringOps, PermutationOps}
import ru.ifmo.onell.util.Permutation

/**
  * This trait encapsulates the knowledge about basic operations with individuals.
  *
  * @tparam MutableIndividual the type of an individual.
  */
trait HasIndividualOperations[MutableIndividual] {
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
  def initializeRandomly(individual: MutableIndividual, rng: Random): Unit

  /**
    * Copies one individual into another one.
    * @param source the individual whose contents are copied.
    * @param destination the individual which receives the contents.
    */
  def copy(source: MutableIndividual, destination: MutableIndividual): Unit
}

/**
  * This companion object contains several known implementations of the `HasIndividualOperations` trait.
  */
object HasIndividualOperations {
  implicit def forBooleanArray: HasIndividualOperations[Array[Boolean]] = BitStringOps
  implicit def forPermutation: HasIndividualOperations[Permutation] = PermutationOps
}
