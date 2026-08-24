package rummikub.model

import rummikub.model.Piece.{Fixed, Joker}

class BagSpec extends munit.FunSuite:

  private def red(n: Int): Fixed = Fixed(Colour.Red, n)

  test("toString orders jokers first, then by number, then by colour Red, Blue, Black, Yellow") {
    val bag = Bag(List(Joker, Joker, Fixed(Colour.Yellow, 3), red(3), Fixed(Colour.Black, 3), Fixed(Colour.Blue, 3), red(5)))
    assertEquals(bag.toString, "Joker x 2, Red 3, Blue 3, Black 3, Yellow 3, Red 5")
  }

  test("toString renders a single piece without a count") {
    assertEquals(Bag(List(Joker, red(7))).toString, "Joker, Red 7")
  }

  test("takeRandomPiece on an empty bag yields nothing") {
    assertEquals(Bag.empty.takeRandomPiece, None)
  }

  test("takeRandomPiece removes exactly the returned piece from the bag") {
    val original = Bag(List(red(3), Fixed(Colour.Blue, 4), Joker))
    original.takeRandomPiece.fold(fail("expected a piece")) { case (piece, rest) =>
      assert(original.hasPiece(piece))
      assertEquals(rest.piecesAsVector.size, 2)
      assertEquals(rest.add(piece, 1), original)
    }
  }

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
