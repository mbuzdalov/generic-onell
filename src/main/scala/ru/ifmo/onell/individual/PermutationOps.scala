package ru.ifmo.onell.individual

import ru.ifmo.onell.HasIndividualOperations
import ru.ifmo.onell.util.Permutation

import java.util.random.RandomGenerator

/**
  * This object contains individual operations for permutations.
  */
object PermutationOps extends HasIndividualOperations[Permutation]:
  override def createStorage(problemSize: Int): Permutation = Permutation.identity(problemSize)
  override def initializeRandomly(individual: Permutation, rng: RandomGenerator): Unit = Permutation.shuffle(individual, rng)
