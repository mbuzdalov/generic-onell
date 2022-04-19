package ru.ifmo.onell.util.par

trait Executor[T] extends AutoCloseable {
  def addSynchronousCallback(fun: T => Unit): Unit
  def addTask(fun: => T): Unit
}
