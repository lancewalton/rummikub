package rummikub.model

import cats.data.NonEmptyList
import rummikub.model.Piece.{Fixed, Joker}

class GroupSpec extends munit.FunSuite:

  private def run(pieces: Piece*): Group.Run = Group.Run(NonEmptyList.fromListUnsafe(pieces.toList))
  private def set(pieces: Piece*): Group.Number = Group.Number(NonEmptyList.fromListUnsafe(pieces.toList))
  private def red(n: Int): Fixed = Fixed(Colour.Red, n)

  test("a three-tile run of consecutive same-colour numbers is valid") {
    assert(run(red(3), red(4), red(5)).isValid)
  }

  test("a two-tile run is too short to be valid") {
    assert(!run(red(3), red(4)).isValid)
  }

  test("a run whose tiles are not all the same colour is invalid") {
    assert(!run(red(3), Fixed(Colour.Blue, 4), red(5)).isValid)
  }

  test("a run with a gap in the numbers is invalid") {
    assert(!run(red(3), red(4), red(6)).isValid)
  }

  test("a run may not extend beyond 13") {
    assert(!run(red(12), red(13), Fixed(Colour.Red, 14)).isValid)
  }

  test("a joker may fill a gap in a run") {
    assert(run(red(3), Joker, red(5)).isValid)
  }

  test("a joker representing a number beyond 13 makes a run invalid") {
    assert(!run(red(12), red(13), Joker).isValid)
  }

  private def coloured(colour: Colour, n: Int): Fixed = Fixed(colour, n)

  test("a three-tile set of one number in distinct colours is valid") {
    assert(set(coloured(Colour.Red, 7), coloured(Colour.Blue, 7), coloured(Colour.Black, 7)).isValid)
  }

  test("a four-tile set of one number in all colours is valid") {
    assert(set(coloured(Colour.Red, 7), coloured(Colour.Blue, 7), coloured(Colour.Black, 7), coloured(Colour.Yellow, 7)).isValid)
  }

  test("a set with a repeated colour is invalid") {
    assert(!set(coloured(Colour.Red, 7), coloured(Colour.Blue, 7), coloured(Colour.Red, 7)).isValid)
  }

  test("a set with differing numbers is invalid") {
    assert(!set(coloured(Colour.Red, 7), coloured(Colour.Blue, 8), coloured(Colour.Black, 7)).isValid)
  }

  test("a two-tile set is too short to be valid") {
    assert(!set(coloured(Colour.Red, 7), coloured(Colour.Blue, 7)).isValid)
  }

  test("a set of more than four tiles is invalid") {
    assert(!set(coloured(Colour.Red, 7), coloured(Colour.Blue, 7), coloured(Colour.Black, 7), coloured(Colour.Yellow, 7), Joker).isValid)
  }

  test("a joker may complete a set") {
    assert(set(coloured(Colour.Red, 7), coloured(Colour.Blue, 7), Joker).isValid)
  }
