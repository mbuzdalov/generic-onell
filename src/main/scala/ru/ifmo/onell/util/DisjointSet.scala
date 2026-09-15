package ru.ifmo.onell.util

import java.util.{Arrays => JArrays}

import scala.annotation.tailrec

class DisjointSet(n: Int):
  private val parent, rank = Array.ofDim[Int](n)

  clear()

  def clear(): Unit =
    JArrays.fill(rank, 0)
    Helpers.fillIdentity(parent)

  def get(a: Int): Int =
    val p = fetchParent(a)
    promoteParent(a, p)
    p

  def unite(a: Int, b: Int): Boolean =
    val pa = get(a)
    val pb = get(b)
    if pa != pb then
      if rank(pa) == rank(pb) then rank(pa) += 1

      if rank(pa) < rank(pb) 
      then parent(pa) = pb
      else parent(pb) = pa
    pa != pb

  @tailrec
  private def fetchParent(a: Int): Int =
    val p = parent(a)
    if a == p then a else fetchParent(p)

  @tailrec
  private def promoteParent(a: Int, newParent: Int): Unit =
    val p = parent(a)
    if a != p then
      parent(a) = newParent
      promoteParent(p, newParent)
end DisjointSet
