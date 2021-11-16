package ru.ifmo.onell.util.par

import java.io.PrintWriter

class SequentialExecutor(pw: PrintWriter, prefix: String, sep: String, suffix: String) extends Executor[String] {
  private[this] var isFirst = true

  override def addTask(fun: => String): Unit = {
    val line = (if (isFirst) prefix else sep) + fun
    isFirst = false
    pw.print(line)
    pw.flush()
    print(line)
  }

  override def close(): Unit = {
    pw.print(suffix)
  }
}
