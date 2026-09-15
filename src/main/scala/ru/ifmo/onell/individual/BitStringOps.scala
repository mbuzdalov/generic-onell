package ru.ifmo.onell.individual

import ru.ifmo.onell.HasIndividualOperations

import java.util.random.RandomGenerator

/**
  * This object contains individual operations for bit strings.
  */
object BitStringOps extends HasIndividualOperations[Array[Boolean]]:
  override def createStorage(problemSize: Int): Array[Boolean] = Array.ofDim(problemSize)
  override def initializeRandomly(individual: Array[Boolean], rng: RandomGenerator): Unit =
    val size = individual.length
    var i = 0
    var seed = -1
    while (i < size) {
      if (i & 31) == 0 then seed = rng.nextInt()
      individual(i) = (seed & 1) == 0
      seed >>>= 1
      i += 1
    }
