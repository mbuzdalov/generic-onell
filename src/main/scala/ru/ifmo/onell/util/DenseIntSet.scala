package ru.ifmo.onell.util

final class DenseIntSet(maxElement: Int) extends OrderedSet[Int] {
  private[this] val contained: Array[Boolean] = Array.ofDim(maxElement)
  private[this] val elements: Array[Int] = Array.ofDim(maxElement)
  private[this] var mySize = 0

  override def size: Int = mySize
  override def apply(index: Int): Int = elements(index)

  override def clear(): Unit = {
    var i = 0
    val iMax = mySize
    val cntd = contained
    val elms = elements
    while (i < iMax) {
      cntd(elms(i)) = false
      i += 1
    }
    mySize = 0
  }

  override def add(element: Int): Unit = {
    val ei = element.toInt
    if (!contained(ei)) {
      contained(ei) = true
      elements(mySize) = ei
      mySize += 1
    }
  }
}
