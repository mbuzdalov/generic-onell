package ru.ifmo.onell.util.par

class Multiplexer[T](base: Executor[T], times: Int) extends Executor[T] {
  override def close(): Unit = {}
  override def addTask(fun: => T): Unit = {
    for (_ <- 0 until times) {
      base.addTask(fun)
    }
  }
}
