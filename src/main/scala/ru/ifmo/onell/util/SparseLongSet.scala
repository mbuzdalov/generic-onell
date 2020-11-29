package ru.ifmo.onell.util

import java.util.Random

class SparseLongSet extends OrderedSet[Long] {
  private[this] var elements: Array[Long] = new Array(10)
  private[this] var nElements = 0
  private[this] var hashTable: Array[Long] = new Array(32)
  private[this] var busyIndices: Array[Int] = new Array(10)

  override def size: Int = nElements

  override def apply(index: Int): Long = elements(index)

  override def clear(): Unit = {
    while (nElements > 0) {
      nElements -= 1
      hashTable(busyIndices(nElements)) = 0
    }
  }

  override def add(element: Long): Unit = {
    val idx = indexOfElement(element)
    if (hashTable(idx) == 0) {
      if (nElements == elements.length) {
        elements = Array.copyOf(elements, 2 * elements.length)
        hashTable = new Array(2 * hashTable.length)
        busyIndices = new Array(elements.length)
        rehash()
        insertAtIndex(element, indexOfElement(element), nElements)
      } else {
        insertAtIndex(element, idx, nElements)
      }
      elements(nElements) = element
      nElements += 1
    }
  }

  override def shuffleOrder(rng: Random): Unit = {
    var i = 0
    while (i < nElements) {
      val j = rng.nextInt(i + 1)
      if (j != i) {
        val tmp = elements(i)
        elements(i) = elements(j)
        elements(j) = tmp
      }
      i += 1
    }
  }

  private def insertAtIndex(element: Long, index: Int, nElements: Int): Unit = {
    hashTable(index) = element + 1
    busyIndices(nElements) = index
  }

  private def indexOfElement(element: Long): Int = {
    val look = element + 1
    val mask = hashTable.length - 1

    @scala.annotation.tailrec
    def scan(index: Int): Int = {
      val elem = hashTable(index)
      if (elem == 0 || elem == look) index else scan((index + 1) & mask)
    }

    scan((element * 43).toInt & mask)
  }

  private def rehash(): Unit = {
    var i = 0
    while (i < nElements) {
      val element = elements(i)
      insertAtIndex(element, indexOfElement(element), i)
      i += 1
    }
  }
}
