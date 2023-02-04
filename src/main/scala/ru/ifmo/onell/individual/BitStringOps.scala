package ru.ifmo.onell.individual

import java.util.concurrent.{ThreadLocalRandom => Random}

import ru.ifmo.onell.HasIndividualOperations

/**
  * This object contains individual operations for bit strings.
  */
object BitStringOps extends HasIndividualOperations[Array[Boolean]] {
  override def createStorage(problemSize: Int): Array[Boolean] = new Array(problemSize)
  override def initializeRandomly(individual: Array[Boolean], rng: Random): Unit = {
    val size = individual.length
    var i = 0
    var seed = -1
    while (i < size) {
      if ((i & 31) == 0) {
        seed = rng.nextInt()
      }
      individual(i) = (seed & 1) == 0
      seed >>>= 1
      i += 1
    }
  }
  override def copy(source: Array[Boolean], destination: Array[Boolean]): Unit =
    System.arraycopy(source, 0, destination, 0, source.length)
}
