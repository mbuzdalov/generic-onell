package ru.ifmo.onell.distribution

import java.io.PrintWriter
import java.util.concurrent.ThreadLocalRandom

import com.fasterxml.jackson.core.{JsonFactory, JsonGenerator}

object Performance {
  def runAndPrint(distribution: IntegerDistribution, name: String, nRuns: Int, np: Double, out: JsonGenerator): Unit = {
    var sum = 0.0
    val time0 = System.nanoTime()
    var count = nRuns
    val rng = ThreadLocalRandom.current()
    while (count > 0) {
      sum += distribution.sample(rng)
      count -= 1
    }
    val time = (System.nanoTime() - time0) * 1e-9 / nRuns
    out.writeStartObject()
    out.writeStringField("impl", name)
    out.writeNumberField("time", time)
    out.writeNumberField("time over np", time / np)
    out.writeNumberField("value", sum / nRuns)
    out.writeEndObject()
  }

  private val ns = Seq(1, 2, 3, 4, 5, 6, 7, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536)
  private val normalizedIterations = 5000000
  private def ps(n: Int): Seq[Double] =
    Seq(1.0 / n / math.sqrt(n), 0.5 / n, 1.0 / n, 2.0 / n, 4.0 / n,
        0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.11, 0.12, 0.2, 0.3, 0.4, 0.5,
        0.6, 0.7, 0.8, 0.88, 0.89, 0.9, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99,
        1 - 4.0 / n, 1 - 2.0 / n, 1 - 1.0 / n, 1 - 0.5 / n,
        1 - 1.0 / n / math.sqrt(n)).filter(p => p > 0 && p < 1).distinct.sorted

  def evaluate(filename: String, distributions: Seq[(String, Double => Boolean, (Int, Double) => IntegerDistribution)]): Unit = {
    val json: JsonGenerator = new JsonFactory().createGenerator(new PrintWriter(filename))
    json.writeStartArray()
    for (n <- ns) {
      println(s"Running n = $n")
      json.writeStartObject()
      json.writeNumberField("n", n)
      json.writeArrayFieldStart("_")
      for (p <- ps(n)) {
        println(s"  Running p = $p")
        json.writeStartObject()
        json.writeNumberField("p", p)
        json.writeArrayFieldStart("_")
        for ((name, predicate, distGen) <- distributions) {
          val distribution = distGen(n, p)
          if (predicate(p))
            for (_ <- 0 until 10)
              runAndPrint(distribution, name, normalizedIterations / n, n * p, json)
        }
        json.writeEndArray()
        json.writeEndObject()
      }
      json.writeEndArray()
      json.writeEndObject()
    }
    json.writeEndArray()
    json.close()
  }

  def main(args: Array[String]): Unit =
    args(0) match {
      case "standard" => evaluate("binomial-performance-standard.json", Seq(
        ("default",  _ => true, (n, p) => BinomialDistribution.standard(n, p)),
        ("naive",    _ => true, (n, p) => new BinomialDistribution.StandardByDefinition(n, p)),
        ("scanner+", _ <= 0.6,  (n, p) => new BinomialDistribution.StandardWithScanner(n, p)),
        ("scanner-", _ >= 0.4,  (n, p) => new BinomialDistribution.StandardWithScannerInverted(n, p)),
        ("inverse+", _ => true, (n, p) => new BinomialDistribution.StandardWithInverseTransformation(n, p)),
        ("inverse-", _ => true, (n, p) => new BinomialDistribution.StandardWithInverseTransformationInverted(n, p)),
      ))
      case "shift" => evaluate("binomial-performance-shift.json", Seq(
        ("default",  _ => true, (n, p) => BinomialDistribution.shift(n, p)),
        ("naive",    _ => true, (n, p) => new BinomialDistribution.ShiftByDefinition(n, p)),
        ("scanner+", _ <= 0.6,  (n, p) => new BinomialDistribution.ShiftWithScanner(n, p)),
        ("scanner-", _ >= 0.4,  (n, p) => new BinomialDistribution.ShiftWithScannerInverted(n, p)),
        ("inverse+", _ => true, (n, p) => new BinomialDistribution.ShiftWithInverseTransformation(n, p)),
        ("inverse-", _ => true, (n, p) => new BinomialDistribution.ShiftWithInverseTransformationInverted(n, p)),
      ))
      case "resampling" => evaluate("binomial-performance-resampling.json", Seq(
        ("default",  _ => true, (n, p) => BinomialDistribution.resampling(n, p)),
        ("naive",    _ => true, (n, p) => new BinomialDistribution.ResamplingByDefinition(n, p)),
        ("scanner+", _ <= 0.6,  (n, p) => new BinomialDistribution.ResamplingWithScanner(n, p)),
        ("scanner-", _ >= 0.4,  (n, p) => new BinomialDistribution.ResamplingWithScannerInverted(n, p)),
        ("naive+",   _ => true, (n, p) => new BinomialDistribution.ResamplingByDefinitionWithSpecialPrefix(n, p))
      ))
      case other => System.err.println(s"Don't know the run configuration '$other'")
    }
}
