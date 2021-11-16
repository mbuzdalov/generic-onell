package ru.ifmo.onell.util.par

import java.util.concurrent.{Executors, TimeUnit}

class ParallelExecutorFT(nThreads: Int) extends Executor[Unit] {
  private[this] val nCPUs = if (nThreads >= 1) nThreads else Runtime.getRuntime.availableProcessors()
  private[this] val pool = Executors.newWorkStealingPool(nCPUs)

  override def addTask(fun: => Unit): Unit = pool.execute(() => fun)

  override def close(): Unit = {
    pool.shutdown()
    pool.awaitTermination(365, TimeUnit.DAYS)
  }
}
