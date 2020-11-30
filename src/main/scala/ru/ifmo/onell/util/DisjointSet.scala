package ru.ifmo.onell.util

import java.util.{Arrays => JArrays}

import scala.annotation.tailrec

class DisjointSet(n: Int) {
  private[this] val parent, rank = new Array[Int](n)

  clear()

  def clear(): Unit = {
    JArrays.fill(rank, 0)
    Helpers.fillIdentity(parent)
  }

  def get(a: Int): Int = {
    val p = fetchParent(a)
    promoteParent(a, p)
    p
  }

  def unite(a: Int, b: Int): Boolean = {
    val pa = get(a)
    val pb = get(b)
    if (pa != pb) {
      if (rank(pa) == rank(pb))
        rank(pa) += 1
      if (rank(pa) < rank(pb))
        parent(pa) = pb
      else
        parent(pb) = pa
    }
    pa != pb
  }

  @tailrec
  private[this] def fetchParent(a: Int): Int = {
    val p = parent(a)
    if (a == p) a else fetchParent(p)
  }

  @tailrec
  private[this] def promoteParent(a: Int, newParent: Int): Unit = {
    val p = parent(a)
    if (a != p) {
      parent(a) = newParent
      promoteParent(p, newParent)
    }
  }
}
