package ru.ifmo.onell.distribution

import java.util.concurrent.ThreadLocalRandom

object Performance {
  def runAndPrint(distribution: IntegerDistribution, name: String, nRuns: Int, np: Double): Unit = {
    var sum = 0.0
    val time0 = System.nanoTime()
    var count = nRuns
    val rng = ThreadLocalRandom.current()
    while (count > 0) {
      sum += distribution.sample(rng)
      count -= 1
    }
    val time = (System.nanoTime() - time0) * 1e-9 / nRuns
    print(s"""{"impl":"$name","time":$time,"time over np":${time / np},"value":${sum / nRuns}}""")
  }

  def main(args: Array[String]): Unit = {
    val normalizedIterations = 5000000
    println("[")
    val ns = Seq(1, 2, 3, 4, 5, 6, 7, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536)
    for (n <- ns) {
      if (n != ns.head) print("},")
      println(s"""{"n":$n,"_":[""")
      val ps = Seq(0.5 / n, 1.0 / n, 2.0 / n, 4.0 / n,
                   0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.11, 0.12, 0.2, 0.3, 0.4, 0.5,
                   0.6, 0.7, 0.8, 0.88, 0.89, 0.9, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99,
                   1 - 4.0 / n, 1 - 2.0 / n, 1 - 1.0 / n, 1 - 0.5 / n).filter(p => p > 0 && p < 1).distinct.sorted
      for (p <- ps) {
        if (p != ps.head) print("},")
        println(s"""{"p":$p,"_":[""")
        for (_ <- 0 until 10) {
          runAndPrint(new BinomialDistribution.WithScanner(n, p), "scanner", normalizedIterations / n, n * p)
          println(",")
        }
        for (_ <- 0 until 10) {
          runAndPrint(new BinomialDistribution.ByDefinition(n, p), "naive", normalizedIterations / n, n * p)
          println(",")
        }
        for (t <- 0 until 10) {
          runAndPrint(BinomialDistribution(n, p), "default", normalizedIterations / n, n * p)
          println(if (t == 9) "]" else ",")
        }
      }
      println("}]")
    }
    println("}]")
  }
}
