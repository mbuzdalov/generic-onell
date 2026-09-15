package ru.ifmo.onell.util

trait OrderedSet[T]:
  def size: Int
  def apply(index: Int): T
  def clear(): Unit
  def add(element: T): Unit

  def copyFrom(that: OrderedSet[T]): Unit =
    clear()
    val thatSize = that.size
    var i = 0
    while i < thatSize do
      add(that(i))
      i += 1
