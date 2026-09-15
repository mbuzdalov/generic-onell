package ru.ifmo.onell.util

import java.util.random.RandomGenerator

final class Permutation private (data: Array[Int]):
  def apply(index: Int): Int = data(index)
  def size: Int = data.length

  def swap(a: Int, b: Int): Unit =
    val tmp = data(a)
    data(a) = data(b)
    data(b) = tmp

object Permutation:
  def identity(size: Int): Permutation = Permutation(Array.tabulate(size)(scala.Predef.identity))

  def shuffle(permutation: Permutation, rng: RandomGenerator): Unit =
    val n = permutation.size
    var i = 1
    while (i < n) {
      val j = rng.nextInt(i + 1)
      if i != j then permutation.swap(i, j)
      i += 1
    }
