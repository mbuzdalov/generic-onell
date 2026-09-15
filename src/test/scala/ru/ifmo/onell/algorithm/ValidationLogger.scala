package ru.ifmo.onell.algorithm

import ru.ifmo.onell.IterationLogger

class ValidationLogger extends IterationLogger[Int]:
  private var lastEvaluations: Long = 0
  private var lastFitness: Int = -1

  def evaluations: Long = lastEvaluations
  def fitness: Int = lastFitness

  override def logIteration(evaluations: Long, fitness: Int): Unit =
    if lastFitness == -1 then assert(evaluations == 1)
    lastFitness = fitness
    assert(lastEvaluations < evaluations)
    lastEvaluations = evaluations
