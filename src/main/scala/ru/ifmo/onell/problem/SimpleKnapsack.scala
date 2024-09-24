package ru.ifmo.onell.problem

import java.io.{BufferedReader, InputStreamReader}
import java.util.concurrent.ThreadLocalRandom
import java.util.zip.GZIPInputStream

import scala.util.Using

import ru.ifmo.onell.util.OrderedSet
import ru.ifmo.onell.{Fitness, HasIndividualOperations}

class SimpleKnapsack(weights: Array[Int], values: Array[Int], maxWeight: Int, val bestFitness: Int, val name: String)
  extends Fitness[SimpleKnapsack.Individual, Int, Int] with HasIndividualOperations[SimpleKnapsack.Individual] {

  private[this] val sumWeights = weights.sum

  override def evaluate(individual: SimpleKnapsack.Individual): Int = individual.fitness
  override def compare(lhs: Int, rhs: Int): Int = java.lang.Integer.compare(lhs, rhs)
  override def worstFitness: Int = -sumWeights
  override def problemSize: Int = weights.length
  override def isOptimalFitness(fitness: Int): Boolean = fitness == bestFitness

  override def numberOfChanges: Int = weights.length
  override def changeIndexTypeToLong(st: Int): Long = st
  override def applyDelta(ind: SimpleKnapsack.Individual,
                          delta: OrderedSet[Int],
                          currentFitness: Int): Int = {
    unapplyDelta(ind, delta)
    ind.fitness
  }

  override def unapplyDelta(ind: SimpleKnapsack.Individual,
                            delta: OrderedSet[Int]): Unit = {
    val size = delta.size
    var i = 0
    while (i < size) {
      ind.flip(delta(i), weights, values)
      i += 1
    }
  }

  override def createStorage(problemSize: Int): SimpleKnapsack.Individual =
    new SimpleKnapsack.Individual(problemSize, maxWeight)

  override def initializeRandomly(individual: SimpleKnapsack.Individual, rng: ThreadLocalRandom): Unit = {
    val size = this.problemSize
    var i = 0
    while (i < size) {
      if (rng.nextBoolean()) {
        individual.flip(i, weights, values)
      }
      i += 1
    }
  }
}

object SimpleKnapsack {
  final class Individual(problemSize: Int, maxWeight: Int) {
    private[this] val bits = new Array[Boolean](problemSize)
    private[this] var sumWeights, sumValues = 0

    def fitness: Int = if (sumWeights > maxWeight) -sumWeights else sumValues

    def flip(index: Int, weights: Array[Int], values: Array[Int]): Unit = {
      if (bits(index)) {
        bits(index) = false
        sumWeights -= weights(index)
        sumValues -= values(index)
      } else {
        bits(index) = true
        sumWeights += weights(index)
        sumValues += values(index)
      }
    }
  }

  val allProblems: Iterator[SimpleKnapsack] = {
    val instanceNamesLarge = for {
      t <- 1 to 3
      sz <- Seq(100, 200, 500)
    } yield s"knapPI_${t}_${sz}_1000_1"

    val instanceNames = instanceNamesLarge
    new Iterator[SimpleKnapsack] {
      private[this] var index: Int = 0
      override def hasNext: Boolean = index < instanceNames.size

      override def next(): SimpleKnapsack = {
        val instanceName = instanceNames(index)
        index += 1
        Using.resource(classOf[SimpleKnapsack].getResourceAsStream(s"/instances/knapsack/$instanceName.gz")) { s =>
          Using.resource(new GZIPInputStream(s)) { gz =>
            Using.resource(new InputStreamReader(gz)) { isr =>
              Using.resource(new BufferedReader(isr)) { br =>
                val s"$nStr $sumWStr" = br.readLine()
                val n = nStr.toInt
                val weights, values = new Array[Int](n)
                for (i <- 0 until n) {
                  val s"$vStr $wStr" = br.readLine()
                  weights(i) = wStr.toInt
                  values(i) = vStr.toInt
                }
                val line = br.readLine()
                val bestValue = if (line == null) Int.MaxValue else {
                  val selection = line.split(" ").map(_.toInt)
                  selection.indices.map(i => selection(i) * values(i)).sum
                }
                new SimpleKnapsack(weights, values, sumWStr.toInt, bestValue, instanceName)
              }
            }
          }
        }
      }
    }
  }
}
