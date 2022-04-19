package ru.ifmo.onell.util.par

import scala.collection.mutable.ArrayBuffer

class SequentialExecutor[T] extends Executor[T] {
  private[this] val callbacks = new ArrayBuffer[T => Unit]()
  override def close(): Unit = {}
  override def addSynchronousCallback(fun: T => Unit): Unit = callbacks += fun
  override def addTask(fun: => T): Unit = {
    val result = fun
    callbacks.foreach(_(result))
  }
}
