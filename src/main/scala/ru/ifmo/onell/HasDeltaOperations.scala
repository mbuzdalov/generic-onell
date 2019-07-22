package ru.ifmo.onell

import java.util.Random

import ru.ifmo.onell.delta.IntArraySetOps
import ru.ifmo.onell.util.IntArraySet

/**
  * This trait encapsulates the knowledge about manipulations with delta representations.
  *
  * @tparam DeltaRepresentation the type of the delta representation.
  */
trait HasDeltaOperations[DeltaRepresentation] {
  /**
    * Creates a new delta representation, given the problem size.
    * @param problemSize the problem size.
    * @return the newly created delta representation.
    */
  def createStorage(problemSize: Int): DeltaRepresentation

  /**
    * Initializes the given delta using some default size distribution law.
    * @param delta the delta to be initialized.
    * @param problemSize the problem size.
    * @param expectedSize the expected delta size.
    * @param rng the random number generator.
    * @return the size of the just-initialized delta.
    */
  def initializeDeltaWithDefaultSize(delta: DeltaRepresentation, problemSize: Int, expectedSize: Double, rng: Random): Int

  /**
    * Initializes the given delta using the specified delta size.
    * @param delta the delta to be initialized.
    * @param problemSize the problem size.
    * @param size the size which the delta must have.
    * @param rng the random number generator.
    */
  def initializeDeltaWithGivenSize(delta: DeltaRepresentation, problemSize: Int, size: Int, rng: Random): Unit

  /**
    * Initializes the given delta using some default distribution law, but taking
    * @param delta the delta to be initialized.
    * @param source the source delta; the initialized delta shall be a subset of the source delta.
    * @param expectedSize the expected delta size.
    * @param rng the random number generator.
    * @return the size of the just-initialized delta.
    */
  def initializeDeltaFromExisting(delta: DeltaRepresentation, source: DeltaRepresentation,
                                  expectedSize: Double, rng: Random): Int
}

/**
  * This companion object contains several known implementations of the `HasDeltaOperations` trait.
  */
object HasDeltaOperations {
  implicit def forIntArraySet: HasDeltaOperations[IntArraySet] = IntArraySetOps
}
