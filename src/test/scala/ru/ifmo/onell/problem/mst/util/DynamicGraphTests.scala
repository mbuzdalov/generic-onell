package ru.ifmo.onell.problem.mst.util

import java.util.Random

import scala.annotation.tailrec
import scala.collection.mutable.{HashSet => MuHashSet}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DynamicGraphTests extends AnyFlatSpec with Matchers {
  private sealed trait ScriptCommand {
    def execute(graph: DynamicGraph): Unit
  }
  private class ScriptAdd(edge: DynamicGraph.Edge) extends ScriptCommand {
    override def execute(graph: DynamicGraph): Unit = graph.addEdge(edge)
  }
  private class ScriptRemove(edge: DynamicGraph.Edge) extends ScriptCommand {
    override def execute(graph: DynamicGraph): Unit = graph.removeEdge(edge)
  }
  private class ScriptTest(expected: Int) extends ScriptCommand {
    override def execute(graph: DynamicGraph): Unit = graph.nConnectivityComponents shouldBe expected
  }
  private object ScriptPrint extends ScriptCommand {
    override def execute(graph: DynamicGraph): Unit = print(graph.nConnectivityComponents.toString + ", ")
  }

  private sealed trait InputCommand
  private case class InputAdd(a: Int, b: Int) extends InputCommand
  private case class InputRemove(a: Int, b: Int) extends InputCommand
  private case class InputTest(expected: Int) extends InputCommand

  private class Script(inputs: InputCommand*) {
    private val allPairs = inputs flatMap {
      case InputAdd(a, b) => Some(math.min(a, b) -> math.max(a, b))
      case InputRemove(a, b) => Some(math.min(a, b) -> math.max(a, b))
      case InputTest(_) => None
    }
    private val edgeMap = allPairs.distinct.zipWithIndex.map(p => p._1 -> new DynamicGraph.Edge {
      override def id: Int = p._2
      override def vertexA: Int = p._1._1
      override def vertexB: Int = p._1._2
    }).toMap
    private val edges = edgeMap.values.toIndexedSeq
    private val commands = inputs map {
      case InputAdd(a, b) => new ScriptAdd(edgeMap(math.min(a, b) -> math.max(a, b)))
      case InputRemove(a, b) => new ScriptRemove(edgeMap(math.min(a, b) -> math.max(a, b)))
      case InputTest(expected) => if (expected < 0) ScriptPrint else new ScriptTest(expected)
    }
    private val nVertices = inputs.map{
      case InputAdd(a, b) => math.max(a, b)
      case InputRemove(a, b) => math.max(a, b)
      case InputTest(_) => -1
    }.max + 1

    def run(factory: DynamicGraph.Factory): Unit = {
      val graph = factory.newEmptyGraph(nVertices, edges)
      commands.foreach(_.execute(graph))
    }
  }

  @tailrec
  private def generateWhileExists(edgeSet: MuHashSet[(Int, Int)], rng: Random, n: Int): (Int, Int) = {
    val a, b = rng.nextInt(n)
    val e = math.min(a, b) -> math.max(a, b)
    if (a == b || edgeSet.contains(e))
      generateWhileExists(edgeSet, rng, n)
    else {
      edgeSet.add(e)
      e
    }
  }

  private def genRand(seed: Long, n: Int, k: Int, p: Int, answerSeq: Int*): Script = {
    val sequence = IndexedSeq.newBuilder[InputCommand]
    val rng = new Random(seed)
    val edgeList = new Array[(Int, Int)](k)
    val edgeSet = new MuHashSet[(Int, Int)]()
    var en = 0
    val maxEdgesPossible = n * (n - 1L) / 2
    var idx = 0
    while (idx < k) {
      if (en == maxEdgesPossible || en > 0 && rng.nextInt(1000) >= p) {
        val index = rng.nextInt(en)
        sequence += InputRemove(edgeList(index)._1, edgeList(index)._2)
        edgeSet.remove(edgeList(index))
        en -= 1
        edgeList(index) = edgeList(en)
      } else {
        val edge = generateWhileExists(edgeSet, rng, n)
        edgeList(en) = edge
        en += 1
        sequence += InputAdd(edge._1, edge._2)
      }
      sequence += InputTest(answerSeq(idx))
      idx += 1
    }

    new Script(sequence.result() :_*)
  }

  private def genRandSeq(seed: Long, n: Int, k: Int, p: Int, answerSeq: Int*): Script = {
    val sequence = IndexedSeq.newBuilder[InputCommand]
    val rng = new Random(seed)
    val edgeList = new Array[(Int, Int)](k)
    val edgeSet = new MuHashSet[(Int, Int)]()
    var st, en = 0
    val maxEdgesPossible = n * (n - 1L) / 2
    var idx = 0
    while (idx < k) {
      if (en - st == maxEdgesPossible || en > st && rng.nextInt(1000) >= p) {
        sequence += InputRemove(edgeList(st)._1, edgeList(st)._2)
        edgeSet.remove(edgeList(st))
        st += 1
      } else {
        val edge = generateWhileExists(edgeSet, rng, n)
        edgeList(en) = edge
        en += 1
        sequence += InputAdd(edge._1, edge._2)
      }
      sequence += InputTest(answerSeq(idx))
      idx += 1
    }

    new Script(sequence.result() :_*)
  }

  @tailrec
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  private def genCircle(n: Int, k: Int): Script = {
    val sequence = IndexedSeq.newBuilder[InputCommand]

    for (t <- 1 to k) {
      val gg = gcd(n, t)
      for (i <- 0 until n) {
        sequence += InputAdd(i, (i + t) % n)
        sequence += InputTest(math.max(n - i - 1, gg))
      }
      for (i <- 0 until n) {
        sequence += InputRemove(i, (i + t) % n)
        sequence += InputTest(math.max(i + 1, gg))
      }
    }

    new Script(sequence.result() :_*)
  }

  private def genTwoFullGraphs(seed: Long, n: Int, k: Int): Script = {
    val sequence = IndexedSeq.newBuilder[InputCommand]
    val rng = new Random(seed)

    for (t <- 0 until 2; i <- 1 until n; j <- 0 until i)
      sequence += InputAdd(i + n * t, j + n * t)

    for (_ <- 0 until k) {
      val a1, a2 = rng.nextInt(n)
      val c1, c2 = rng.nextInt(n - 1)
      val b1 = if (c1 >= a1) c1 + 1 else c1
      val b2 = if (c2 >= a1) c2 + 1 else c2

      sequence += InputAdd(a1, a2 + n)
      sequence += InputTest(1)
      sequence += InputAdd(b1, b2 + n)
      sequence += InputTest(1)
      sequence += InputRemove(a1, a2 + n)
      sequence += InputTest(1)
      sequence += InputRemove(b1, b2 + n)
      sequence += InputTest(2)
    }

    new Script(sequence.result() :_*)
  }

  private def test(name: String, factory: DynamicGraph.Factory): Unit = {
    name should "pass first manual test" in
      new Script(InputTest(5), InputAdd(0, 1), InputAdd(1, 2), InputAdd(2, 3), InputAdd(3, 4), InputAdd(4, 0),
                 InputTest(1), InputRemove(1, 2), InputTest(1), InputRemove(3, 4), InputTest(2)).run(factory)

    it should "pass second manual test" in
      new Script(InputTest(2), InputAdd(0, 1), InputTest(1), InputRemove(1, 0), InputTest(2)).run(factory)

    it should "pass third manual test" in
      new Script(InputAdd(1, 2), InputAdd(1, 3), InputAdd(0, 4), InputAdd(0, 5), InputTest(2), InputAdd(4, 5),
                 InputAdd(2, 3), InputTest(2), InputAdd(0, 1), InputTest(1), InputRemove(0, 1), InputTest(2),
                 InputAdd(2, 4), InputTest(1), InputTest(1), InputAdd(4, 3), InputTest(1)).run(factory)

    it should "pass random test with n=10, k=100, p=500" in
      genRand(23917, 10, 100, 500,
        9, 8, 9, 10, 9, 10, 9, 10, 9, 10, 9, 10, 9, 10, 9, 10, 9, 8, 7, 8, 9, 10, 9, 10, 9, 8, 7, 8, 9, 10,
              9, 8, 9, 10, 9, 10, 9, 10, 9, 10, 9, 8, 7, 6, 5, 4, 5, 6, 5, 4, 4, 4, 3, 3, 4, 5, 4, 3, 4, 3, 3, 3, 4, 3,
              4, 5, 4, 4, 5, 6, 7, 8, 7, 6, 7, 8, 7, 6, 7, 6, 5, 4, 5, 6, 7, 6, 7, 8, 7, 6, 5, 6, 7, 6, 7, 8, 9, 10,
              9, 8).run(factory)

    it should "pass random test with n=10, k=100, p=100" in
      genRand(12321455, 10, 100, 100,
              9, 10, 9, 10, 9, 10, 9, 10, 9, 10, 9, 10, 9, 10, 9, 10, 9, 10, 9, 10, 9, 10, 9, 8, 7, 6, 7, 6,
              7, 8, 9, 10, 9, 10, 9, 8, 9, 10, 9, 10, 9, 8, 9, 10, 9, 10, 9, 10, 9, 10, 9, 10, 9, 8, 9, 10, 9, 10, 9,
              10, 9, 10, 9, 10, 9, 10, 9, 10, 9, 8, 9, 10, 9, 10, 9, 10, 9, 10, 9, 10, 9, 10, 9, 10, 9, 10, 9, 10, 9,
              10, 9, 10, 9, 10, 9, 8, 9, 8, 9, 10).run(factory)

    it should "pass random test with n=10, k=100, p=900" in
      genRand(12321455, 10, 100, 900,
              9, 8, 7, 6, 5, 4, 5, 4, 4, 3, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
              1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
              1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
              1, 1).run(factory)

    it should "pass random test with n=100, k=1000, p=500" in
      genRand(23917, 100, 1000, 500,
              99, 98, 99, 100, 99, 100, 99, 100, 99, 100, 99, 100, 99, 100, 99, 100, 99, 98, 97, 98, 99, 100,
              99, 100, 99, 98, 97, 98, 99, 100, 99, 98, 99, 100, 99, 100, 99, 100, 99, 100, 99, 98, 97, 96, 95, 94, 95,
              96, 95, 94, 93, 94, 93, 92, 91, 92, 93, 92, 93, 94, 93, 92, 93, 94, 93, 94, 95, 94, 95, 96, 97, 98, 99,
              98, 97, 98, 99, 98, 99, 98, 99, 98, 97, 96, 97, 98, 99, 98, 99, 100, 99, 98, 97, 96, 97, 96, 97, 96, 95,
              96, 97, 96, 95, 94, 95, 96, 97, 96, 97, 96, 95, 96, 97, 96, 95, 94, 95, 96, 95, 94, 93, 94, 95, 94, 93,
              92, 93, 94, 95, 94, 95, 94, 93, 92, 91, 92, 91, 92, 91, 92, 93, 94, 95, 96, 97, 98, 97, 98, 97, 98, 99,
              100, 99, 98, 97, 98, 99, 98, 97, 98, 97, 98, 99, 100, 99, 98, 99, 98, 97, 98, 97, 96, 95, 94, 95, 94, 93,
              92, 91, 92, 93, 94, 95, 96, 95, 94, 95, 96, 97, 96, 95, 94, 93, 94, 95, 94, 95, 96, 95, 96, 95, 94, 93,
              92, 93, 92, 93, 94, 95, 94, 93, 92, 91, 90, 91, 92, 93, 94, 93, 94, 93, 94, 93, 92, 93, 94, 95, 94, 95,
              96, 95, 96, 97, 98, 97, 98, 97, 96, 95, 96, 95, 94, 95, 96, 97, 96, 97, 98, 97, 98, 99, 98, 99, 100, 99,
              100, 99, 98, 99, 98, 99, 100, 99, 100, 99, 98, 97, 98, 99, 98, 99, 100, 99, 100, 99, 98, 99, 100, 99, 100,
              99, 100, 99, 98, 99, 98, 97, 96, 97, 96, 95, 94, 95, 96, 97, 98, 99, 98, 97, 98, 97, 96, 97, 96, 97, 98,
              99, 98, 99, 98, 99, 100, 99, 98, 97, 98, 99, 100, 99, 100, 99, 100, 99, 100, 99, 100, 99, 100, 99, 98, 99,
              98, 97, 98, 97, 98, 97, 96, 95, 94, 95, 96, 95, 96, 95, 94, 95, 94, 95, 94, 95, 94, 93, 94, 93, 94, 93,
              92, 93, 94, 93, 94, 93, 94, 95, 96, 95, 96, 95, 94, 95, 94, 95, 94, 93, 92, 91, 92, 93, 92, 91, 90, 89,
              88, 87, 88, 89, 90, 91, 92, 91, 92, 93, 94, 93, 94, 95, 94, 95, 96, 97, 98, 97, 98, 97, 98, 99, 100, 99,
              98, 99, 100, 99, 100, 99, 100, 99, 100, 99, 100, 99, 98, 97, 98, 97, 98, 99, 100, 99, 98, 97, 96, 95, 96,
              95, 94, 95, 96, 97, 96, 95, 96, 95, 96, 97, 98, 97, 96, 95, 94, 95, 94, 95, 96, 95, 94, 93, 94, 93, 94,
              93, 92, 93, 92, 93, 92, 91, 90, 91, 90, 89, 90, 89, 90, 89, 88, 87, 86, 87, 88, 89, 88, 89, 88, 87, 88,
              87, 88, 89, 88, 89, 90, 89, 88, 89, 88, 87, 86, 87, 88, 87, 88, 89, 88, 89, 88, 89, 90, 89, 90, 91, 92,
              93, 94, 95, 96, 95, 96, 95, 94, 93, 92, 91, 92, 91, 90, 89, 88, 89, 90, 91, 92, 93, 92, 91, 92, 93, 92,
              91, 92, 91, 92, 91, 92, 93, 92, 91, 90, 91, 90, 91, 90, 91, 92, 91, 92, 93, 94, 93, 94, 95, 96, 95, 96,
              95, 94, 93, 92, 91, 92, 93, 92, 93, 94, 95, 96, 97, 96, 97, 98, 97, 96, 97, 96, 97, 98, 99, 98, 99, 98,
              97, 98, 97, 96, 95, 96, 95, 96, 97, 98, 99, 100, 99, 100, 99, 98, 97, 96, 97, 96, 95, 96, 95, 94, 95, 94,
              95, 94, 93, 92, 93, 92, 93, 92, 93, 94, 93, 92, 93, 92, 93, 94, 93, 94, 95, 94, 93, 92, 93, 92, 93, 92,
              93, 92, 91, 92, 91, 90, 91, 90, 89, 90, 91, 92, 91, 92, 91, 92, 93, 94, 95, 96, 95, 94, 93, 92, 93, 92,
              91, 92, 93, 92, 93, 92, 91, 92, 93, 92, 93, 92, 91, 92, 91, 92, 93, 92, 93, 92, 91, 92, 93, 94, 93, 94,
              95, 96, 95, 96, 95, 96, 97, 96, 97, 98, 99, 98, 99, 98, 99, 98, 97, 98, 99, 98, 99, 100, 99, 100, 99, 98,
              99, 100, 99, 98, 97, 98, 99, 98, 99, 100, 99, 98, 99, 98, 99, 100, 99, 98, 97, 98, 99, 100, 99, 98, 99,
              98, 99, 100, 99, 98, 99, 100, 99, 100, 99, 98, 97, 98, 99, 98, 99, 98, 99, 100, 99, 100, 99, 98, 99, 98,
              99, 100, 99, 98, 99, 100, 99, 100, 99, 100, 99, 98, 97, 96, 97, 96, 97, 98, 97, 98, 99, 98, 97, 98, 97,
              98, 97, 98, 97, 96, 95, 96, 97, 98, 97, 98, 99, 100, 99, 98, 97, 98, 97, 98, 97, 98, 97, 96, 95, 94, 95,
              96, 95, 94, 95, 94, 95, 94, 93, 92, 91, 92, 91, 90, 89, 88, 87, 88, 87, 86, 87, 88, 89, 88, 89, 90, 91,
              90, 91, 92, 91, 90, 89, 88, 89, 90, 91, 92, 93, 94, 95, 94, 93, 92, 93, 94, 93, 94, 95, 96, 95, 96, 97,
              96, 95, 94, 95, 96, 97, 98, 97, 96, 97, 96, 97, 96, 95, 94, 95, 94, 93, 94, 95, 94, 95, 94, 95, 96, 97,
              98, 97, 96, 95, 96, 97, 96, 95, 96, 95, 96, 95, 96, 97, 98, 97, 98, 97, 96, 97, 96, 97, 96, 95, 94, 93,
              92, 93, 92, 93, 94, 93, 94, 93, 92, 91, 92, 91, 92, 91, 90, 89, 90, 89, 90, 91, 90, 91, 92, 91, 92, 91,
              90, 91, 90, 89, 88, 89, 90, 91, 92, 91, 90, 89, 88, 89, 90, 89, 90, 91, 90, 89, 90, 91, 92, 91, 92, 91,
              90, 91, 90, 91, 90, 91, 90, 89, 90, 89, 88, 89, 88, 87, 86, 85, 84, 83, 82, 81, 82, 83, 84).run(factory)

    it should "pass random test with n=100, k=1000, p=600" in
      genRand(8235923462L, 100, 1000, 600,
              99, 98, 97, 96, 97, 96, 95, 94, 93, 92, 91, 92, 91, 90, 89, 90, 89, 88, 87, 88, 87, 86, 87, 88,
              89, 88, 87, 86, 87, 88, 87, 88, 89, 90, 89, 88, 87, 88, 89, 90, 89, 90, 89, 90, 89, 88, 87, 86, 85, 84,
              83, 82, 81, 82, 83, 82, 83, 84, 83, 84, 83, 82, 83, 84, 85, 86, 85, 84, 83, 82, 81, 82, 81, 80, 79, 78,
              77, 76, 75, 76, 75, 76, 75, 76, 75, 74, 73, 72, 73, 72, 71, 72, 71, 70, 69, 68, 67, 68, 69, 68, 69, 68,
              69, 70, 69, 70, 71, 72, 71, 70, 69, 68, 67, 68, 69, 68, 67, 68, 69, 68, 67, 66, 67, 66, 67, 68, 69, 68,
              67, 66, 65, 64, 63, 62, 61, 62, 61, 60, 59, 60, 61, 60, 59, 60, 59, 58, 57, 58, 57, 56, 55, 56, 55, 56,
              55, 56, 57, 56, 57, 58, 57, 58, 59, 60, 61, 60, 59, 58, 57, 56, 55, 54, 55, 56, 55, 54, 53, 54, 53, 54,
              53, 52, 51, 52, 53, 54, 53, 52, 51, 52, 53, 52, 51, 50, 50, 51, 50, 49, 50, 51, 50, 50, 51, 50, 49, 50,
              49, 48, 49, 49, 48, 49, 50, 51, 51, 52, 53, 54, 53, 52, 51, 50, 49, 48, 49, 49, 48, 49, 48, 49, 50, 49,
              50, 49, 50, 49, 49, 50, 49, 48, 47, 48, 48, 47, 46, 46, 45, 44, 45, 44, 44, 45, 44, 43, 44, 45, 46, 47,
              46, 47, 46, 47, 47, 48, 47, 48, 47, 46, 45, 44, 45, 46, 45, 46, 45, 44, 45, 46, 47, 46, 47, 48, 47, 48,
              49, 48, 49, 48, 47, 46, 47, 47, 48, 47, 48, 49, 50, 51, 50, 49, 50, 51, 52, 51, 50, 49, 49, 49, 48, 49,
              48, 47, 48, 48, 47, 46, 45, 46, 45, 45, 44, 44, 43, 42, 41, 42, 42, 43, 42, 41, 40, 41, 40, 41, 42, 43,
              42, 41, 40, 39, 38, 39, 40, 39, 40, 39, 39, 39, 40, 41, 40, 40, 39, 38, 38, 39, 38, 39, 38, 37, 38, 37,
              38, 37, 36, 35, 36, 35, 36, 35, 34, 35, 34, 33, 34, 35, 36, 35, 36, 35, 34, 34, 33, 34, 33, 32, 33, 34,
              35, 36, 35, 34, 35, 34, 34, 33, 34, 33, 32, 32, 31, 32, 33, 33, 32, 31, 32, 31, 30, 30, 29, 28, 27, 26,
              26, 25, 24, 23, 24, 24, 23, 24, 25, 25, 25, 24, 24, 25, 25, 24, 24, 23, 24, 23, 22, 22, 21, 21, 22, 22,
              23, 24, 23, 23, 23, 24, 25, 25, 25, 25, 24, 25, 24, 23, 24, 25, 25, 25, 25, 24, 25, 26, 25, 26, 26, 25,
              26, 27, 27, 26, 26, 25, 25, 24, 23, 24, 25, 25, 25, 25, 26, 27, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
              28, 28, 28, 28, 29, 28, 28, 27, 26, 26, 25, 24, 25, 25, 26, 26, 25, 25, 24, 23, 23, 23, 23, 24, 24, 25,
              24, 23, 23, 24, 23, 23, 22, 23, 23, 24, 24, 23, 24, 24, 24, 23, 22, 21, 21, 21, 21, 21, 20, 20, 21, 21,
              21, 20, 20, 20, 20, 21, 21, 21, 21, 22, 22, 21, 21, 21, 21, 21, 21, 21, 21, 22, 22, 23, 22, 22, 22, 22,
              22, 22, 22, 21, 21, 21, 22, 22, 23, 23, 22, 22, 21, 21, 21, 20, 19, 19, 19, 19, 19, 20, 20, 19, 19, 19,
              18, 18, 18, 18, 18, 18, 17, 18, 18, 18, 18, 19, 18, 17, 17, 17, 17, 16, 16, 16, 16, 16, 15, 16, 16, 16,
              16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 17, 17, 17, 16, 17, 17, 17, 17, 16, 16, 16, 16, 16, 16,
              15, 15, 14, 14, 14, 14, 14, 14, 15, 16, 16, 16, 16, 16, 16, 15, 15, 14, 14, 15, 15, 14, 14, 15, 15, 15,
              15, 15, 15, 15, 15, 15, 16, 16, 16, 16, 16, 16, 16, 15, 15, 15, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
              14, 14, 14, 14, 14, 14, 14, 15, 15, 15, 16, 16, 15, 15, 16, 16, 17, 17, 18, 17, 18, 17, 17, 16, 15, 16,
              15, 16, 16, 16, 16, 16, 15, 14, 15, 15, 15, 16, 16, 17, 17, 17, 17, 17, 18, 18, 18, 19, 20, 20, 20, 20,
              20, 21, 20, 20, 20, 20, 20, 20, 20, 20, 21, 21, 20, 19, 20, 19, 20, 20, 20, 19, 19, 20, 20, 19, 19, 19,
              19, 19, 19, 20, 20, 19, 18, 17, 17, 17, 17, 18, 19, 18, 19, 19, 19, 19, 19, 18, 18, 17, 17, 16, 16, 17,
              16, 17, 17, 18, 18, 18, 18, 18, 17, 17, 17, 17, 17, 16, 16, 16, 16, 17, 17, 16, 17, 17, 17, 18, 18, 18,
              18, 17, 17, 17, 18, 18, 17, 18, 18, 18, 18, 18, 19, 18, 18, 18, 19, 19, 20, 19, 18, 18, 18, 19, 19, 18,
              18, 18, 19, 19, 19, 19, 19, 19, 19, 18, 18, 19, 19, 19, 19, 20, 20, 20, 19, 18, 19, 19, 18, 18, 17, 17,
              16, 17, 16, 15, 15, 15, 16, 15, 15, 14, 13, 13, 13, 13, 13, 13, 13, 12, 13, 14, 14, 13, 12, 12, 12, 12,
              12, 12, 12, 12, 12, 12, 12, 13, 14, 14, 14, 14, 13, 13, 13, 13, 12, 12, 13, 13, 13, 12, 12, 12, 12, 12,
              12, 12, 13, 13, 13, 13, 13, 12, 12, 11, 12, 12, 12, 12, 12, 11, 11, 11, 10, 10, 10, 10, 9, 9, 9, 9, 9,
              9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
              10, 10, 9, 9, 9, 9, 9, 9, 9).run(factory)

    it should "pass random sequential removal test with n=10, k=100, p=500" in
      genRandSeq(23917, 10, 100, 500,
                 9, 8, 9, 10, 9, 10, 9, 8, 9, 8, 9, 10, 9, 10, 9, 10, 9, 10, 9, 10, 9, 8, 7, 8, 7, 8, 9, 8,
                 9, 8, 7, 6, 5, 6, 7, 8, 7, 6, 7, 6, 5, 5, 6, 6, 5, 6, 5, 6, 5, 4, 3, 4, 5, 6, 5, 6, 5, 4, 5, 6, 5, 4,
                 3, 4, 5, 6, 5, 4, 4, 3, 3, 4, 5, 4, 3, 3, 4, 5, 6, 5, 6, 5, 5, 5, 6, 5, 5, 4, 4, 3, 3, 3, 3, 3, 3, 3,
                 3, 2, 2, 2).run(factory)

    it should "pass random sequential removal test with n=10, k=100, p=600" in
      genRandSeq(24358243634L, 10, 100, 600,
                 9, 8, 9, 8, 7, 8, 9, 8, 7, 8, 9, 10, 9, 8, 7, 8, 7, 8, 7, 8, 9, 8, 9, 10, 9, 8, 7, 6, 7, 6,
                 7, 6, 7, 6, 5, 6, 5, 6, 7, 8, 7, 7, 7, 8, 7, 6, 5, 6, 5, 6, 5, 4, 3, 4, 5, 5, 5, 6, 5, 4, 4, 5, 4, 4,
                 3, 2, 2, 1, 1, 1, 2, 2, 3, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1).run(factory)

    it should "pass random test with n=100, k=1000, p=800" in
      genRandSeq(82345346345L, 100, 1000, 800,
                 99, 100, 99, 98, 97, 96, 95, 94, 93, 92, 93, 92, 91, 90, 89, 88, 87, 86, 85, 84, 83, 82, 81,
                 80, 79, 78, 77, 76, 77, 78, 77, 76, 75, 74, 73, 72, 71, 70, 69, 68, 67, 66, 65, 64, 63, 64, 63, 62, 61,
                 60, 59, 58, 59, 60, 59, 58, 57, 56, 55, 56, 55, 54, 53, 52, 51, 52, 51, 50, 49, 48, 47, 46, 45, 44, 43,
                 44, 43, 42, 41, 42, 43, 42, 43, 42, 41, 42, 43, 42, 41, 42, 41, 40, 40, 40, 39, 40, 40, 40, 39, 40, 39,
                 38, 37, 38, 37, 36, 35, 34, 34, 35, 35, 34, 33, 32, 31, 31, 31, 30, 29, 28, 27, 26, 25, 24, 24, 23, 22,
                 22, 23, 22, 21, 21, 21, 21, 22, 21, 21, 21, 21, 21, 21, 20, 20, 20, 20, 20, 19, 19, 19, 19, 20, 20, 20,
                 19, 18, 18, 18, 18, 17, 17, 16, 16, 16, 16, 16, 16, 16, 15, 15, 14, 13, 13, 14, 13, 13, 13, 13, 12, 12,
                 12, 13, 13, 13, 13, 13, 13, 13, 13, 12, 13, 12, 12, 11, 11, 11, 12, 12, 12, 12, 11, 11, 11, 12, 12, 12,
                 12, 11, 11, 10, 10, 9, 9, 9, 9, 9, 9, 9, 9, 9, 8, 8, 8, 7, 7, 7, 7, 7, 7, 7, 7, 6, 6, 6, 6, 6, 6, 6, 6,
                 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 6, 6,
                 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
                 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
                 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1).run(factory)

    it should "pass circle with n=100, k=10" in genCircle(100, 10).run(factory)
    it should "pass circle with n=99, k=10" in genCircle(99, 10).run(factory)
    it should "pass circle with n=128, k=10" in genCircle(128, 10).run(factory)
    it should "pass circle with n=144, k=12" in genCircle(144, 12).run(factory)

    it should "pass test with two full graphs and occasional connections with n=37" in
      genTwoFullGraphs(734853463, 37, 2000).run(factory)
    it should "pass test with two full graphs and occasional connections with n=100" in
      genTwoFullGraphs(234354366, 100, 1000).run(factory)
  }

  test("Naive implementation", NaiveDynamicGraph)
  test("Implementation by Vitaly Aksenov improved by MB", AksenovDynamicGraphWrapper)
}
