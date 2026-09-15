package ru.ifmo.onell.util

import scala.annotation.tailrec

class SparseLongSet extends OrderedSet[Long]:
  private var elements: Array[Long] = Array.ofDim(10)
  private var nElements = 0
  private var hashTable: Array[Long] = Array.ofDim(32)
  private var busyIndices: Array[Int] = Array.ofDim(10)

  override def size: Int = nElements

  override def apply(index: Int): Long = elements(index)

  override def clear(): Unit =
    while nElements > 0 do
      nElements -= 1
      hashTable(busyIndices(nElements)) = 0

  override def add(element: Long): Unit =
    val idx = indexOfElement(element)
    if hashTable(idx) == 0 then
      if nElements == elements.length then
        elements = Array.copyOf(elements, 2 * elements.length)
        hashTable = Array.ofDim(2 * hashTable.length)
        busyIndices = Array.ofDim(elements.length)
        rehash()
        insertAtIndex(element, indexOfElement(element), nElements)
      else insertAtIndex(element, idx, nElements)
      elements(nElements) = element
      nElements += 1

  private def insertAtIndex(element: Long, index: Int, nElements: Int): Unit =
    hashTable(index) = element + 1
    busyIndices(nElements) = index

  private def indexOfElement(element: Long): Int =
    val look = element + 1
    val mask = hashTable.length - 1

    @tailrec def scan(index: Int): Int =
      val elem = hashTable(index)
      if elem == 0 || elem == look then index else scan((index + 1) & mask)

    scan((element * 43).toInt & mask)

  private def rehash(): Unit =
    var i = 0
    while i < nElements do
      val element = elements(i)
      insertAtIndex(element, indexOfElement(element), i)
      i += 1
