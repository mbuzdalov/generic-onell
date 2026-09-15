package ru.ifmo.onell

trait IterationLogger[F]:
  def logIteration(evaluations: Long, fitness: F): Unit

object IterationLogger:
  def dummyLogger[F]: IterationLogger[F] = (_: Long, _: F) => {}
