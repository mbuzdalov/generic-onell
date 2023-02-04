package ru.ifmo.onell

import ru.ifmo.onell.util.OrderedSet
import ru.ifmo.onell.util.Specialization.{changeSpecialization => cs, fitnessSpecialization => fs}

/**
  * This trait encapsulates the knowledge for how to compute fitness,
  * and how to quickly compute the new fitness based on the old one
  * and the description of the change. The individual is assumed to be mutable.
  *
  * @tparam IndividualType the type of the individual.
  * @tparam FitnessType the type of the fitness value.
  * @tparam ChangeIndexType the type of the index of a single change (e.g. the index to flip a bit at).
  */
trait Fitness[IndividualType, @specialized(fs) FitnessType, @specialized(cs) ChangeIndexType] {
  /**
    * Evaluates the given individual.
    * @param individual the individual.
    * @return the fitness.
    */
  def evaluate(individual: IndividualType): FitnessType

  /**
    * Compares the given fitness values.
    * @param lhs the left-hand-side fitness.
    * @param rhs the right-hand-side fitness.
    * @return zero if fitness values are equal; negative if `lhs` is worse; positive if `lhs` is greater.
    */
  def compare(lhs: FitnessType, rhs: FitnessType): Int

  /**
    * Returns the maximum of the two fitness values.
    * @param lhs the left-hand-side fitness.
    * @param rhs the right-hand-side fitness.
    * @return the maximum of the two fitness values.
    */
  final def max(lhs: FitnessType, rhs: FitnessType): FitnessType = if (compare(lhs, rhs) < 0) rhs else lhs

  /**
    * Returns the worst value of the fitness, which is suitable to e.g. initialize best seen fitness.
    * This method is allowed to return an illegal fitness value, e.g. -1 for OneMax,
    * which would still compare perfectly fine with all other fitness values.
    *
    * @return the worst value of the fitness.
    */
  def worstFitness: FitnessType

  /**
    * Returns the problem size.
    * @return the problem size.
    */
  def problemSize: Int

  /**
    * Tests whether the given fitness is optimal.
    * @param fitness the fitness value.
    * @return `true` if the fitness is optimal, `false` otherwise.
    */
  def isOptimalFitness(fitness: FitnessType): Boolean

  /**
    * Returns the number of possible changes.
    * @return the number of possible changes.
    */
  def numberOfChanges: ChangeIndexType

  /**
    * Convert the change size type to long.
    * @param st the value of the change size type.
    * @return its long value.
    */
  def changeIndexTypeToLong(st: ChangeIndexType): Long

  /**
    * Applies the given delta to the given individual, while simultaneously recomputing the fitness.
    * @param ind the (mutable) individual.
    * @param delta the delta (the description of a change to the individual).
    * @param currentFitness the fitness of the individual before applying the change.
    * @return the new fitness after applying the change; the individual is also changed when the method returns.
    */
  def applyDelta(ind: IndividualType, delta: OrderedSet[ChangeIndexType], currentFitness: FitnessType): FitnessType

  /**
    * Unapplies the given delta to the given individual.
    * @param ind the (mutable) individual, which has previously experienced applying exactly the same delta.
    * @param delta the delta to be unapplied.
    */
  def unapplyDelta(ind: IndividualType, delta: OrderedSet[ChangeIndexType]): Unit

  /**
    * Creates a delta from two individuals: the "from-individual" and the "to-individual".
    * If the resulting delta is applied to this individual, the result shall be identical to the to-individual.
    * The result should be stored in the `destination` set.
    *
    * @param from the from-individual.
    * @param to the to-individual.
    * @param destination the destination to store the delta.
    */
  def fillDelta(from: IndividualType, to: IndividualType, destination: OrderedSet[ChangeIndexType]): Unit

  /**
    * Evaluates the given individual, assuming the delta is applied.
    * The individual shall retain in the original state after applying this operation,
    * although it may be modified during the computation.
    *
    * @param ind the individual to measure assuming delta is applied.
    * @param delta the delta to be applied.
    * @param currentFitness the current fitness of the individual.
    * @return the new fitness after applying the change; the individual remains intact.
    */
  def evaluateAssumingDelta(ind: IndividualType, delta: OrderedSet[ChangeIndexType],
                            currentFitness: FitnessType): FitnessType = {
    val result = applyDelta(ind, delta, currentFitness)
    unapplyDelta(ind, delta)
    result
  }
}
