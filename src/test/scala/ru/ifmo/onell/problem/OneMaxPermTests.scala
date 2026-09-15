package ru.ifmo.onell.problem

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OneMaxPermTests extends AnyFlatSpec with Matchers:
  "OneMaxPerm.unpack" should "work correctly for small inputs" in:
    var change = 0
    var big = 1
    while big < 1000 do
      var small = 0
      while small < big do
        val j1 = OneMaxPerm.getBigger(change)
        val j0 = OneMaxPerm.getSmaller(change, j1)

        j0 shouldBe small
        j1 shouldBe big
        small += 1
        change += 1
      big += 1

  it should "work correctly for large inputs" in:
    val input = 1457569539L
    val bigger = OneMaxPerm.getBigger(input)
    val smaller = OneMaxPerm.getSmaller(input, bigger)
    bigger shouldBe 53992
    smaller shouldBe 28503
