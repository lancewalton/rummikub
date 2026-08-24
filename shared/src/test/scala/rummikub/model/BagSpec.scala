package rummikub.model

import rummikub.model.Piece.Fixed

class BagSpec extends munit.FunSuite:

  private def red(n: Int): Fixed = Fixed(Colour.Red, n)

  test("a bag contains another when it has at least as many of every piece") {
    assert(Bag(red(1), red(1), red(2)).contains(Bag(red(1), red(2))))
  }

  test("a bag does not contain another that needs more copies of a piece") {
    assert(!Bag(red(1)).contains(Bag(red(1), red(1))))
  }

  test("multiset difference removes the given pieces by multiplicity") {
    assertEquals(Bag(red(1), red(1), red(2)) -- Bag(red(1), red(2)), Bag(red(1)))
  }

  test("difference of a piece not present leaves the bag unchanged") {
    assertEquals(Bag(red(1)) -- Bag(red(2)), Bag(red(1)))
  }
