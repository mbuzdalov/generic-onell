package ru.ifmo.onell.individual

import java.util.concurrent.{ThreadLocalRandom => Random}

import ru.ifmo.onell.HasIndividualOperations
import ru.ifmo.onell.util.Permutation

/**
  * This object contains individual operations for permutations.
  */
object PermutationOps extends HasIndividualOperations[Permutation] {
  override def createStorage(problemSize: Int): Permutation = Permutation.identity(problemSize)
  override def initializeRandomly(individual: Permutation, rng: Random): Unit = Permutation.shuffle(individual, rng)
  override def copy(source: Permutation, destination: Permutation): Unit = source.copyTo(destination)
}
