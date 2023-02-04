package ru.ifmo.onell.problem

import java.io.{BufferedReader, IOException, InputStream, InputStreamReader}
import java.util.concurrent.ThreadLocalRandom
import java.util.zip.GZIPInputStream

import scala.util.Using

import ru.ifmo.onell.{Fitness, HasIndividualOperations}
import ru.ifmo.onell.util.{Helpers, OrderedSet}
import MultiDimensionalKnapsack._

class MultiDimensionalKnapsack(bitDefinitions: Array[BitDefinition], weightLimits: Array[Int], val linearRelaxation: Double)
  extends Fitness[Individual, Int, Int] with HasIndividualOperations[Individual]
{
  require(bitDefinitions.forall(_.nWeights == weightLimits.length))

  def nConstraints: Int = weightLimits.length
  val tightnessRatio: Double = {
    val sumWeights = bitDefinitions.view.map(_.weight(0)).sum
    val ratio = weightLimits(0).toDouble / sumWeights
    (ratio * 4 + 0.5).toInt / 4.0
  }

  // trivial methods
  override def worstFitness: Int = -1
  override def problemSize: Int = bitDefinitions.length
  override def numberOfChanges: Int = bitDefinitions.length
  override def isOptimalFitness(fitness: Int): Boolean = false
  override def changeIndexTypeToLong(st: Int): Long = st
  override def compare(lhs: Int, rhs: Int): Int = lhs - rhs

  // fitness calls are mostly delegates to individual with our data as input
  override def evaluate(individual: Individual): Int = individual.fitness
  override def applyDelta(ind: Individual, delta: OrderedSet[Int], currentFitness: Int): Int = {
    val size = delta.size
    var i = 0
    while (i < size) {
      ind.flip(delta(i), bitDefinitions, weightLimits)
      i += 1
    }
    ind.fitness
  }
  override def unapplyDelta(ind: Individual, delta: OrderedSet[Int]): Unit = applyDelta(ind, delta, ind.fitness)
  override def fillDelta(from: Individual, to: Individual, dest: OrderedSet[Int]): Unit = from.diff(to, dest)

  // individual operations are a part of the problem due to greedy initialization
  override def createStorage(problemSize: Int): Individual = new Individual(problemSize)
  override def initializeRandomly(individual: Individual, rng: ThreadLocalRandom): Unit = {
    val bitsToTry = new Array[Int](problemSize)
    var i = 0
    while (i < problemSize) {
      val index = rng.nextInt(i + 1)
      bitsToTry(i) = bitsToTry(index)
      bitsToTry(index) = i
      i += 1
    }
    tryFlipMore(individual, rng, bitsToTry, 0)
  }
  @scala.annotation.tailrec
  private def tryFlipMore(individual: Individual, rng: ThreadLocalRandom, order: Array[Int], i: Int): Unit = {
    if (i < problemSize) {
      val index = order(i)
      individual.flip(index, bitDefinitions, weightLimits)
      if (individual.fitness == 0) {
        individual.flip(index, bitDefinitions, weightLimits)
        // do nothing more, although technically possible
      } else {
        tryFlipMore(individual, rng, order, i + 1)
      }
    }
  }

  override def copy(source: Individual, destination: Individual): Unit = source.copyTo(destination)
}

object MultiDimensionalKnapsack {
  final class BitDefinition(val cost: Int, weights: Array[Int]) {
    def nWeights: Int = weights.length
    def weight(index: Int): Int = weights(index)
  }

  final class Individual(problemSize: Int) {
    private val bits = new Array[Boolean](problemSize)
    private var cost, nViolatedConstraints = 0
    private var weights: Array[Int] = _

    def fitness: Int = if (nViolatedConstraints > 0) 0 else cost

    def copyTo(that: Individual): Unit = {
      System.arraycopy(bits, 0, that.bits, 0, bits.length)
      that.cost = cost
      that.nViolatedConstraints = nViolatedConstraints
      that.weights = weights
    }

    def flip(index: Int, bitDefinitions: Array[BitDefinition], weightLimits: Array[Int]): Unit = {
      if (weights == null) {
        weights = new Array[Int](weightLimits.length)
      }
      if (bits(index)) {
        bits(index) = false
        val definition = bitDefinitions(index)
        cost -= definition.cost
        var i = weightLimits.length
        while (i > 0) {
          i -= 1
          val oldWeight = weights(i)
          val newWeight = oldWeight - definition.weight(i)
          weights(i) = newWeight
          val limit = weightLimits(i)
          if (oldWeight > limit && newWeight <= limit) {
            nViolatedConstraints -= 1
          }
        }
      } else {
        bits(index) = true
        val definition = bitDefinitions(index)
        cost += definition.cost
        var i = weightLimits.length
        while (i > 0) {
          i -= 1
          val oldWeight = weights(i)
          val newWeight = oldWeight + definition.weight(i)
          weights(i) = newWeight
          val limit = weightLimits(i)
          if (oldWeight <= limit && newWeight > limit) {
            nViolatedConstraints += 1
          }
        }
      }
    }

    def diff(that: Individual, destination: OrderedSet[Int]): Unit =
      Helpers.findDifferingBits(bits, that.bits, destination)
  }

  lazy val ChuBeaselyProblems: Seq[MultiDimensionalKnapsack] = {
    Using.resource(classOf[MultiDimensionalKnapsack].getResourceAsStream(s"/instances/mkp/relaxations.txt")) { rel =>
      Using.resource(new BufferedReader(new InputStreamReader(rel))) { relLines =>
        (1 to 9) flatMap { i =>
          Using.resource(classOf[MultiDimensionalKnapsack].getResourceAsStream(s"/instances/mkp/mknapcb$i.txt.gz")) { gz =>
            Using.resource(new GZIPInputStream(gz))(parseProblems(relLines))
          }
        }
      }
    }
  }

  private def parseProblems(relaxationLineReader: BufferedReader)(stream: InputStream): Seq[MultiDimensionalKnapsack] = {
    val reader = new IntReader(stream)
    val nProblems = reader.nextInt()
    Seq.fill(nProblems)(parseProblem(reader, relaxationLineReader))
  }

  private def parseProblem(reader: IntReader, relaxationLineReader: BufferedReader): MultiDimensionalKnapsack = {
    val problemSize, nWeights, _ = reader.nextInt()
    val costs = Array.fill(problemSize)(reader.nextInt())
    val weights = Array.fill(nWeights, problemSize)(reader.nextInt())
    val maxWeights = Array.fill(nWeights)(reader.nextInt())
    val bitDefinitions = Array.tabulate(problemSize)(i => new BitDefinition(costs(i), Array.tabulate(nWeights)(j => weights(j)(i))))
    new MultiDimensionalKnapsack(bitDefinitions, maxWeights, relaxationLineReader.readLine().toDouble)
  }

  private class IntReader(stream: InputStream) {
    @scala.annotation.tailrec
    private def collect(before: Boolean, sign: Int, result: Int): Int = {
      val byte = stream.read()
      if (byte < 0) {
        if (before) throw new IOException("Unexpected end of file")
        sign * result
      } else if (byte <= ' ') {
        if (before) collect(before, sign, result) else result * sign
      } else if (byte == '-') {
        if (before) collect(before, -sign, result) else throw new IOException("A minus in the middle of the number")
      } else if (byte >= '0' && byte <= '9') {
        collect(before = false, sign, result * 10 + byte - '0')
      } else {
        throw new IOException("Illegal byte in an int-only file: " + byte)
      }
    }

    def nextInt(): Int = collect(before = true, 1, 0)
  }
}
