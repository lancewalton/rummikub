package rumikub.ai

import cats.data.NonEmptyList
import rumikub.model.*

class NumberGroupsSuite extends munit.FunSuite {
  private val black = piece(Colour.Black)
  private val blue = piece(Colour.Blue)
  private val red = piece(Colour.Red)
  private val yellow = piece(Colour.Yellow)

  test("not enough pieces, no jokers") {
    assertEquals(NumberGroups(Bag(black, blue)), Set.empty)
  }

  test("three colours, no jokers") {
    assertEquals(NumberGroups(Bag(black, blue, red)), Set(group(black, blue, red)))
  }

  test("four colours, no jokers") {
    assertEquals(
      NumberGroups(Bag(black, blue, red, yellow)),
      Set(
        group(black, blue, red),
        group(black, blue, yellow),
        group(black, red, yellow),
        group(blue, red, yellow),
        group(black, blue, red, yellow)
      ))
  }

  test("duplicate colours, no jokers, no groups") {
    assertEquals(
      NumberGroups(Bag(black, blue, blue)),
      Set.empty
    )
  }

  test("not enough pieces, 1 joker") {
    assertEquals(
      NumberGroups(Bag(Piece.Joker)),
      Set.empty
    )
  }

  test("not enough pieces, 2 jokers") {
    assertEquals(
      NumberGroups(Bag(Piece.Joker, Piece.Joker)),
      Set.empty
    )
  }

  test("two colours, 1 joker") {
    assertEquals(NumberGroups(Bag(black, blue, Piece.Joker)), Set(group(Piece.Joker, black, blue)))
  }

  test("one colours, 2 jokers") {
    assertEquals(NumberGroups(Bag(black, Piece.Joker, Piece.Joker)), Set(group(Piece.Joker, Piece.Joker, black)))
  }

  test("two colours, 2 jokers") {
    assertEquals(
      NumberGroups(Bag(black, blue, Piece.Joker, Piece.Joker)),
      Set(
        group(Piece.Joker, black, blue),
        group(Piece.Joker, Piece.Joker, black),
        group(Piece.Joker, Piece.Joker, blue),
        group(Piece.Joker, Piece.Joker, black, blue)
      )
    )
  }

  test("three colours, 1 joker") {
    assertEquals(
      NumberGroups(Bag(black, blue, red, Piece.Joker)),
      Set(
        group(black, blue, red),
        group(Piece.Joker, black, blue),
        group(Piece.Joker, black, red),
        group(Piece.Joker, blue, red),
        group(Piece.Joker, black, blue, red)
      )
    )
  }

  test("three colours, 2 jokers") {
    assertEquals(
      NumberGroups(Bag(black, blue, red, Piece.Joker, Piece.Joker)),
      Set(
        group(black, blue, red),
        group(Piece.Joker, black, blue),
        group(Piece.Joker, black, red),
        group(Piece.Joker, blue, red),
        group(Piece.Joker, black, blue, red),
        group(Piece.Joker, Piece.Joker, black),
        group(Piece.Joker, Piece.Joker, blue),
        group(Piece.Joker, Piece.Joker, red),
        group(Piece.Joker, Piece.Joker, black, blue),
        group(Piece.Joker, Piece.Joker, black, red),
        group(Piece.Joker, Piece.Joker, blue, red),
      )
    )
  }

  private def group(piece: Piece, rest: Piece*) = Group.Number(NonEmptyList.of(piece, rest*))

  private def piece(colour: Colour) = Piece.Fixed(colour, 1)
}
