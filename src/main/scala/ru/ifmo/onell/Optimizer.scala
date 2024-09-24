package ru.ifmo.onell

import scala.{specialized => sp}
import ru.ifmo.onell.util.Specialization.{fitnessSpecialization => fsp, changeSpecialization => csp}

/**
  * A common trait for optimizers.
  */
trait Optimizer {
  /**
    * Runs optimization for a given problem.
    *
    * @param fitness the problem description, including problem size, termination condition, and fitness evaluation.
    * @param iterationLogger the logger to be notified of fitness improvements.
    * @param deltaOps the implicit that explains how to manage representations of individual deltas.
    * @param indOps the implicit that explains how to manage representations of individuals.
    * @tparam I the type of individual.
    * @tparam F the type of fitness value.
    * @tparam C the type of change index of an individual.
    * @return the number of evaluations until an optimum is found.
    */
  def optimize[I, @sp(fsp) F, @sp(csp) C](fitness: Fitness[I, F, C],
                                          iterationLogger: IterationLogger[F] = IterationLogger.dummyLogger[F])
                                         (implicit deltaOps: HasDeltaOperations[C], indOps: HasIndividualOperations[I]): Long
}
