package ru.ifmo.onell.util.par

import java.util.concurrent.{Executors, TimeUnit}

import scala.collection.mutable.ArrayBuffer

class ParallelExecutor[T](nThreads: Int) extends Executor[T]:
  private val lock = new AnyRef()
  private val callbacks = ArrayBuffer[T => Unit]()
  private val nCPUs = if nThreads >= 1 then nThreads else Runtime.getRuntime.availableProcessors()
  private val pool = Executors.newWorkStealingPool(nCPUs)

  override def addSynchronousCallback(fun: T => Unit): Unit =
    lock.synchronized:
      callbacks += fun

  override def addTask(fun: => T): Unit =
    pool.execute: () =>
      val result = fun
      lock.synchronized:
        callbacks.foreach(_(result))

  override def close(): Unit =
    pool.shutdown()
    while !pool.awaitTermination(365, TimeUnit.DAYS) do ()
