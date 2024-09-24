package ru.ifmo.onell.util

final class DenseIntSet(maxElement: Int) extends OrderedSet[Int] {
  private[this] val contained: Array[Boolean] = Array.ofDim(maxElement)
  private[this] val elements: Array[Int] = Array.ofDim(maxElement)
  private[this] var mySize = 0

  override def size: Int = mySize
  override def apply(index: Int): Int = elements(index)

  override def clear(): Unit = {
    var i = 0
    while (i < mySize) {
      contained(elements(i)) = false
      i += 1
    }
    mySize = 0
  }

  override def add(element: Int): Unit = {
    if (!contained(element)) {
      contained(element) = true
      elements(mySize) = element
      mySize += 1
    }
  }
}
