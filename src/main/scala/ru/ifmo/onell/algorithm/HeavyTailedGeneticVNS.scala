package ru.ifmo.onell.algorithm

import java.util.concurrent.ThreadLocalRandom

import scala.collection.mutable.ArrayBuffer
import scala.{specialized => sp}

import ru.ifmo.onell.distribution.PowerLawDistribution
import ru.ifmo.onell.util.Specialization.{changeSpecialization => csp, fitnessSpecialization => fsp}
import ru.ifmo.onell.{Fitness, HasDeltaOperations, HasIndividualOperations, IterationLogger, Optimizer}

class HeavyTailedGeneticVNS(mutantParentSelectionBeta: Double,
                            mutantDistanceSelectionBeta: Double,
                            crossoverPairSelectionBeta: Double,
                            crossoverDistanceSelectionBeta: Double) extends Optimizer {
  case class FitInd[I, @sp(fsp) F](i: I, f: F)

  class SortedVector[T](beta: Double, limit: Int)(implicit ord: Ordering[T]) {
    private[this] val data = new ArrayBuffer[T]()
    private[this] val prefixSums = new ArrayBuffer[Double]()

    def add(elem: T): Unit = {
      var i = data.size
      data.addOne(elem)
      while (i > 0 && ord.lteq(data(i - 1), elem)) {
        data.update(i, data(i - 1))
        i -= 1
      }
      data.update(i, elem)
      if (data.size > limit) {
        data.remove(data.size - 1)
      } else if (data.size == 1) {
        prefixSums.addOne(1.0)
      } else {
        prefixSums.addOne(prefixSums.last + math.pow(data.size, -beta))
      }
    }

    def map[U](function: T => U): ArrayBuffer[U] = data.map(function)

    def size: Int = data.size

    def get(index: Int): T = data(index)

    def sample(rng: ThreadLocalRandom): T = {
      val value = rng.nextDouble(prefixSums.last)
      var index = 0
      while (prefixSums(index) < value) {
        index += 1
      }
      get(index)
    }
  }

  override def optimize[I, @sp(fsp) F, @sp(csp) C](fitness: Fitness[I, F, C],
                                                   iterationLogger: IterationLogger[F])
                                                  (implicit deltaOps: HasDeltaOperations[C],
                                                   indOps: HasIndividualOperations[I]): Long = {
    val rng = ThreadLocalRandom.current()
    val problemSize = fitness.problemSize
    val nChanges = fitness.numberOfChanges
    val nChangesL = fitness.changeIndexTypeToLong(nChanges)
    val mutantDistanceDistribution = PowerLawDistribution(nChangesL, mutantDistanceSelectionBeta)
    val spareDiffSpace1, spareDiffSpace2 = deltaOps.createStorage(nChanges)

    implicit val fitIndOrdering: Ordering[FitInd[I, F]] = (x: FitInd[I, F], y: FitInd[I, F]) => fitness.compare(x.f, y.f)

    class Population {
      private[this] val population = new SortedVector[FitInd[I, F]](mutantParentSelectionBeta, Int.MaxValue)
      private[this] val pairs = new ArrayBuffer[SortedVector[(FitInd[I, F], FitInd[I, F])]]()
      private[this] val pairBetas = new ArrayBuffer[Double]()
      private[this] var bestFitness: F = _

      def addIndividual(i: I): Boolean = addIndividual(i, fitness.evaluate(i))

      def addIndividual(i: I, f: F): Boolean = {
        val distances = population.map(p => {
          fitness.fillDelta(i, p.i, spareDiffSpace1)
          spareDiffSpace1.size
        })
        if (distances.contains(0)) {
          // Do not add the individual
          false
        } else {
          if (population.size == 0 || fitness.compare(f, bestFitness) > 0) {
            bestFitness = f
          }
          val theInd = FitInd(i, f)
          distances.indices.foreach(i => {
            val distance = distances(i)
            assert(distance > 0)
            while (pairs.size < distance) {
              pairs.addOne(new SortedVector(crossoverPairSelectionBeta, 10))
              if (pairBetas.isEmpty) {
                pairBetas.addOne(1.0)
              } else {
                pairBetas.addOne(pairBetas.last + math.pow(pairs.size, -crossoverDistanceSelectionBeta))
              }
            }
            pairs(distance - 1).add((theInd, population.get(i)))
            pairs(distance - 1).add((population.get(i), theInd))
          })
          population.add(theInd)
          true
        }
      }

      def getBestFitness: F = bestFitness

      def sampleParent(): FitInd[I, F] = population.sample(rng)

      def sampleParentPair(): (FitInd[I, F], FitInd[I, F]) = {
        var chosenIndex = 0
        do {
          val distance = rng.nextDouble(pairBetas.last)
          chosenIndex = 0
          while (pairBetas(chosenIndex) < distance) {
            chosenIndex += 1
          }
        } while (pairs(chosenIndex).size == 0)
        pairs(chosenIndex).sample(rng)
      }
    }

    val population = new Population()
    population.addIndividual({
      val storage = indOps.createStorage(problemSize)
      indOps.initializeRandomly(storage, rng)
      storage
    })
    var evaluations = 1L
    iterationLogger.logIteration(evaluations, population.getBestFitness)

    while (!fitness.isOptimalFitness(population.getBestFitness)) {
      val (child, childFitness) = if (evaluations == 1 || rng.nextBoolean()) {
        // do mutation
        val FitInd(parent, parentFitness) = population.sampleParent()
        val child = indOps.createStorage(problemSize)
        indOps.copy(parent, child)
        val nDesiredChanges = mutantDistanceDistribution.sample(rng)
        deltaOps.initializeDelta(spareDiffSpace1, nChanges, nDesiredChanges, rng)
        (child, fitness.applyDelta(child, spareDiffSpace1, parentFitness))
      } else {
        // do crossover
        val (FitInd(p1, f1), FitInd(p2, _)) = population.sampleParentPair()
        fitness.fillDelta(p1, p2, spareDiffSpace2)
        val distanceDistribution = PowerLawDistribution(spareDiffSpace2.size, mutantDistanceSelectionBeta)
        val distanceSample = distanceDistribution.sample(rng)
        deltaOps.initializeDeltaFromExisting(spareDiffSpace1, spareDiffSpace2, distanceSample, rng)
        val child = indOps.createStorage(problemSize)
        indOps.copy(p1, child)
        (child, fitness.applyDelta(child, spareDiffSpace1, f1))
      }
      if (population.addIndividual(child, childFitness)) {
        evaluations += 1
        iterationLogger.logIteration(evaluations, population.getBestFitness)
      }
    }

    evaluations
  }
}
