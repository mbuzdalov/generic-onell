package ru.ifmo.onell

import java.util.concurrent.{ThreadLocalRandom => Random}

import ru.ifmo.onell.util.{DenseIntSet, OrderedSet, SparseLongSet}
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
    * Samples a random change index out of `nChanges` indices, using the supplied random number generator `rng`.
    * @param nChanges the number of changes to sample from.
    * @param rng the random number generator.
    * @return the particular sampled change.
    */
  protected def sampleRandomChangeIndex(nChanges: ChangeIndexType, rng: Random): ChangeIndexType

    /**
    * Initializes the given delta using the specified delta size.
    * @param delta the delta to be initialized.
    * @param nChanges the number of possible changes.
    * @param size the size which the delta must have.
    * @param rng the random number generator.
    */
  def initializeDelta(delta: OrderedSet[ChangeIndexType], nChanges: ChangeIndexType, size: Int, rng: Random): Unit = {
    delta.clear()
    while (delta.size < size) {
      delta.add(sampleRandomChangeIndex(nChanges, rng))
    }
  }

  /**
    * Initializes the given delta using a subset of the given source delta with the given size.
    * @param delta the delta to be initialized.
    * @param source the source delta; the initialized delta shall be a subset of the source delta.
    * @param size the delta subset size.
    * @param rng the random number generator.
    */
  def initializeDeltaFromExisting(delta: OrderedSet[ChangeIndexType], source: OrderedSet[ChangeIndexType],
                                  size: Int, rng: Random): Unit = {
    delta.clear()
    val sourceSize = source.size
    while (delta.size < size) {
      // TODO: this can be much slower than intended if size is almost sourceSize
      // TODO: order must be preserved!
      delta.add(source(rng.nextInt(sourceSize)))
    }
  }
}

/**
  * This companion object contains several known implementations of the `HasDeltaOperations` trait.
  */
object HasDeltaOperations {
  object IntSetOps extends HasDeltaOperations[Int] {
    override def createStorage(nChanges: Int): OrderedSet[Int] = new DenseIntSet(nChanges)
    override protected def sampleRandomChangeIndex(nChanges: Int, rng: Random): Int = rng.nextInt(nChanges)
  }

  object LongSetOps extends HasDeltaOperations[Long] {
    override def createStorage(nChanges: Long): OrderedSet[Long] = new SparseLongSet()
    override protected def sampleRandomChangeIndex(nChanges: Long, rng: Random): Long = rng.nextLong(nChanges)
  }

  implicit def forInt:  HasDeltaOperations[Int]  = IntSetOps
  implicit def forLong: HasDeltaOperations[Long] = LongSetOps
}
