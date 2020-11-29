package ru.ifmo.onell

import java.util.concurrent.{ThreadLocalRandom => Random}

import ru.ifmo.onell.delta.{IntSetOps, LongSetOps}
import ru.ifmo.onell.util.OrderedSet
import ru.ifmo.onell.util.Specialization.{changeSpecialization => csp}

/**
  * This trait encapsulates the knowledge about manipulations with delta representations.
  *
  * @tparam ChangeIndexType the type of a single element of the delta.
  */
trait HasDeltaOperations[@specialized(csp) ChangeIndexType] {
  /**
    * Creates a new delta representation, given the number of possible changes.
    * @param nChanges the number of possible changes.
    * @return the newly created delta representation.
    */
  def createStorage(nChanges: ChangeIndexType): OrderedSet[ChangeIndexType]

  /**
    * Initializes the given delta using the specified delta size.
    * @param delta the delta to be initialized.
    * @param nChanges the number of possible changes.
    * @param size the size which the delta must have.
    * @param rng the random number generator.
    */
  def initializeDelta(delta: OrderedSet[ChangeIndexType], nChanges: ChangeIndexType, size: Int, rng: Random): Unit

  /**
    * Initializes the given delta using a subset of the given source delta with the given size.
    * @param delta the delta to be initialized.
    * @param source the source delta; the initialized delta shall be a subset of the source delta.
    * @param size the delta subset size.
    * @param rng the random number generator.
    */
  def initializeDeltaFromExisting(delta: OrderedSet[ChangeIndexType], source: OrderedSet[ChangeIndexType],
                                  size: Int, rng: Random): Unit
}

/**
  * This companion object contains several known implementations of the `HasDeltaOperations` trait.
  */
object HasDeltaOperations {
  implicit def forInt:  HasDeltaOperations[Int]  = IntSetOps
  implicit def forLong: HasDeltaOperations[Long] = LongSetOps
}
