package ru.ifmo.onell.util

object Helpers {
  def fillIdentity(array: Array[Int]): Unit = {
    var i = array.length
    while (i > 0) {
      i -= 1
      array(i) = i
    }
  }

  def countTrueBits(individual: Array[Boolean]): Int = {
    var i, rv = 0
    val size = individual.length
    while (i < size) {
      if (individual(i)) rv += 1
      i += 1
    }
    rv
  }

  def countChanges(individual: Array[Boolean], indices: OrderedSet[Int]): Int = {
    val size = indices.size
    var changes = 0
    var i = 0
    while (i < size) {
      val idx = indices(i)
      changes += (if (individual(idx)) -1 else 1)
      i += 1
    }
    changes
  }

  def flipEachBit(individual: Array[Boolean], indices: OrderedSet[Int]): Unit = {
    var i = indices.size - 1
    while (i >= 0) {
      individual(indices(i)) ^= true
      i -= 1
    }
  }
}
