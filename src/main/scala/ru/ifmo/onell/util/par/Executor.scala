package ru.ifmo.onell.util.par

trait Executor[T] extends AutoCloseable {
  def addTask(fun: => T): Unit
}
