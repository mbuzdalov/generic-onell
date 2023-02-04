package ru.ifmo.onell.util

import java.util.Random

final class Permutation private (private val data: Array[Int]) {
  def apply(index: Int): Int = data(index)
  def size: Int = data.length

  def swap(a: Int, b: Int): Unit = {
    val tmp = data(a)
    data(a) = data(b)
    data(b) = tmp
  }

  def copyTo(destination: Permutation): Unit = System.arraycopy(data, 0, destination.data, 0, data.length)
}

object Permutation {
  def identity(size: Int): Permutation = new Permutation(Array.tabulate(size)(scala.Predef.identity))

  def shuffle(permutation: Permutation, rng: Random): Unit = {
    val n = permutation.size
    var i = 1
    while (i < n) {
      val j = rng.nextInt(i + 1)
      if (i != j) {
        permutation.swap(i, j)
      }
      i += 1
    }
  }
}
