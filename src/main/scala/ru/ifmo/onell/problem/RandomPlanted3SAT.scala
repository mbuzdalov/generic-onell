package ru.ifmo.onell.problem

import java.util.Random

import scala.annotation.tailrec

import ru.ifmo.onell.Fitness
import ru.ifmo.onell.util.{DenseIntSet, Helpers, OrderedSet}

class RandomPlanted3SAT(val problemSize: Int, val clauseCount: Int,
                        valueGenerator: RandomPlanted3SAT.ValueGenerator, 
                        randomSeed: Long) extends Fitness[Array[Boolean], Int, Int]:
  private val clauseVar = Array.ofDim[Int](clauseCount * 3)
  private val clauseVal = Array.ofDim[Int](clauseCount)
  private val clausesOfVariableOffset = Array.ofDim[Int](problemSize + 1)
  private val clausesOfVariableContent = Array.ofDim[Int](clauseCount * 3)
  private val usedClauses = DenseIntSet(clauseCount)

  // initialization
  generateClauses(0, Random(randomSeed))
  makePartialSums(0)
  populateClausesOfVariableContent(0)

  private def isClauseSatisfied(clauseIndex: Int, solution: Array[Boolean]): Boolean =
    val clauseOffset = 3 * clauseIndex
    val i0 = clauseOffset
    val i1 = clauseOffset + 1
    val i2 = clauseOffset + 2
    val v0 = if (solution(clauseVar(i0))) 1 else 0
    val v1 = if (solution(clauseVar(i1))) 2 else 0
    val v2 = if (solution(clauseVar(i2))) 4 else 0
    ((v0 ^ v1 ^ v2) ^ clauseVal(clauseIndex)) != 7

  @tailrec
  private def generateClauses(clauseIdx: Int, rng: Random): Unit = if clauseIdx < clauseCount then
    val offset = 3 * clauseIdx
    val i0, i1, i2 = rng.nextInt(problemSize)
    clauseVar(offset) = i0
    clauseVar(offset + 1) = i1
    clauseVar(offset + 2) = i2
    valueGenerator.generateThree(clauseVal, clauseIdx, rng)
    assert(clauseVal(clauseIdx) != 0)
    clausesOfVariableOffset(i0) += 1
    clausesOfVariableOffset(i1) += 1
    clausesOfVariableOffset(i2) += 1
    generateClauses(clauseIdx + 1, rng)

  @tailrec
  private def makePartialSums(i: Int): Unit = if i < problemSize then
    clausesOfVariableOffset(i + 1) += clausesOfVariableOffset(i)
    makePartialSums(i + 1)

  @tailrec
  private def populateClausesOfVariableContent(i: Int): Unit = if i < clauseVar.length then
    val variable = clauseVar(i)
    val offset = clausesOfVariableOffset(variable) - 1
    clausesOfVariableOffset(variable) = offset
    clausesOfVariableContent(offset) = i / 3
    populateClausesOfVariableContent(i + 1)

  override def worstFitness: Int = -1
  override def compare(lhs: Int, rhs: Int): Int = Integer.compare(lhs, rhs)
  override def isOptimalFitness(fitness: Int): Boolean = fitness == clauseCount
  override def numberOfChanges: Int = problemSize
  override def changeIndexTypeToLong(st: Int): Long = st

  override def evaluate(individual: Array[Boolean]): Int =
    (0 until clauseCount).count(i => isClauseSatisfied(i, individual))

  override def applyDelta(ind: Array[Boolean], delta: OrderedSet[Int], currentFitness: Int): Int =
    usedClauses.clear()
    val size = delta.size
    var i = 0
    while i < size do
      val d = delta(i)
      var j = clausesOfVariableOffset(d)
      val jMax = clausesOfVariableOffset(d + 1)
      while j < jMax do
        usedClauses.add(clausesOfVariableContent(j))
        j += 1
      i += 1
    val satBefore = countSatisfiedClauses(ind)
    Helpers.flipEachBit(ind, delta)
    currentFitness - satBefore + countSatisfiedClauses(ind)

  override def unapplyDelta(ind: Array[Boolean], delta: OrderedSet[Int]): Unit = Helpers.flipEachBit(ind, delta)

  private def countSatisfiedClauses(individual: Array[Boolean]): Int =
    var f = 0
    var i = usedClauses.size - 1
    while i >= 0 do
      f += (if isClauseSatisfied(usedClauses(i), individual) then 1 else 0)
      i -= 1
    f
end RandomPlanted3SAT

object RandomPlanted3SAT:
  trait ValueGenerator:
    def generateThree(targetArray: Array[Int], offset: Int, rng: Random): Unit

  object EasyGenerator extends ValueGenerator:
    override def generateThree(targetArray: Array[Int], offset: Int, rng: Random): Unit =
      targetArray(offset) = 1 + rng.nextInt(7)

  object HardGenerator extends ValueGenerator:
    override def generateThree(targetArray: Array[Int], offset: Int, rng: Random): Unit =
      targetArray(offset) = rng.nextInt(6) match
        case 0 => 7
        case 1 => 7 - (1 << rng.nextInt(3))
        case _ => 1 << rng.nextInt(3)
