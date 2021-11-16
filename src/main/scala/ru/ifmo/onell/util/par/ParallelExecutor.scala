package ru.ifmo.onell.util.par

import java.io.PrintWriter
import java.util.concurrent.{Executors, TimeUnit}

class ParallelExecutor(pw: PrintWriter, prefix: String, sep: String, suffix: String, nThreads: Int) extends Executor[String] {
  private[this] val lock = new AnyRef
  private[this] var isFirst = true
  private[this] val nCPUs = if (nThreads >= 1) nThreads else Runtime.getRuntime.availableProcessors()
  private[this] val pool = Executors.newWorkStealingPool(nCPUs)

  override def addTask(fun: => String): Unit = pool.execute(() => {
    val line0 = fun
    lock synchronized {
      val line = (if (isFirst) prefix else sep) + line0
      isFirst = false
      pw.print(line)
      pw.flush()
      print(line)
    }
  })

  override def close(): Unit = {
    pool.shutdown()
    pool.awaitTermination(365, TimeUnit.DAYS)
    pw.print(suffix)
  }
}
