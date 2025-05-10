package rumikub.ai

import cats.data.NonEmptyList
import rumikub.model.*

class SequenceGroupsSuite extends munit.FunSuite {
  private val p1 = piece(1)
  private val p2 = piece(2)
  private val p3 = piece(3)
  private val p4 = piece(4)
  private val p5 = piece(5)
  private val p6 = piece(6)
  private val p7 = piece(7)
  private val p11 = piece(11)
  private val p12 = piece(12)
  private val p13 = piece(13)

  test("not enough pieces") {
    assertEquals(SequenceGroups(Bag(p1, p2)), Set.empty)
  }

  test("enough pieces, but gaps") {
    assertEquals(SequenceGroups(Bag(p1, p2, p4)), Set.empty)
  }

  test("one group, no jokers") {
    assertEquals(SequenceGroups(Bag(p1, p2, p3)), Set(group(p1, p2, p3)))
  }

  test("three overlapping groups, no jokers") {
    assertEquals(SequenceGroups(Bag(p1, p2, p3, p4)), Set(group(p1, p2, p3), group(p2, p3, p4), group(p1, p2, p3, p4)))
  }

  test("two non-overlapping groups, no jokers") {
    assertEquals(SequenceGroups(Bag(p1, p2, p3, p5, p6, p7)), Set(group(p1, p2, p3), group(p5, p6, p7)))
  }

  test("1, 2 and one joker") {
    assertEquals(SequenceGroups(Bag(p1, p2, Piece.Joker)), Set(group(p1, p2, Piece.Joker)))
  }

  test("2, 3 and one joker") {
    assertEquals(SequenceGroups(Bag(p2, p3, Piece.Joker)), Set(group(p2, p3, Piece.Joker), group(Piece.Joker, p2, p3)))
  }

  test("12, 13 and one joker") {
    assertEquals(SequenceGroups(Bag(p12, p13, Piece.Joker)), Set(group(Piece.Joker, p12, p13)))
  }

  test("11, 12 and one joker") {
    assertEquals(
      SequenceGroups(Bag(p11, p12, Piece.Joker)),
      Set(
        group(Piece.Joker, p11, p12),
        group(p11, p12, Piece.Joker)
      ))
  }


  test("two pieces separated by 1, and one joker") {
    assertEquals(SequenceGroups(Bag(p2, p4, Piece.Joker)), Set(group(p2, Piece.Joker, p4)))
  }

  test("2, 4, 5, joker") {
    assertEquals(
      SequenceGroups(Bag(p2, p4, p5, Piece.Joker)),
      Set(
        group(p2, Piece.Joker, p4),
        group(Piece.Joker, p4, p5),
        group(p4, p5, Piece.Joker),
        group(p2, Piece.Joker, p4, p5)
      ))
  }

  test("1 and two jokers") {
    assertEquals(
      SequenceGroups(Bag(p1, Piece.Joker, Piece.Joker)),
      Set(
        group(p1, Piece.Joker, Piece.Joker)
      ))
  }

  test("13 and two jokers") {
    assertEquals(
      SequenceGroups(Bag(p13, Piece.Joker, Piece.Joker)),
      Set(
        group(Piece.Joker, Piece.Joker, p13)
      ))
  }

  test("2 and two jokers") {
    assertEquals(
      SequenceGroups(Bag(p2, Piece.Joker, Piece.Joker)),
      Set(
        group(Piece.Joker, p2, Piece.Joker),
        group(p2, Piece.Joker, Piece.Joker)
      ))
  }

  test("12 and two jokers") {
    assertEquals(
      SequenceGroups(Bag(p12, Piece.Joker, Piece.Joker)),
      Set(
        group(Piece.Joker, p12, Piece.Joker),
        group(Piece.Joker, Piece.Joker, p12)
      ))
  }

  test("1, 3 and two jokers") {
    assertEquals(
      SequenceGroups(Bag(p1, p3, Piece.Joker, Piece.Joker)),
      Set(
        group(p1, Piece.Joker, p3),
        group(p1, Piece.Joker, Piece.Joker),
        group(p1, Piece.Joker, p3, Piece.Joker),
        group(Piece.Joker, Piece.Joker, p3),
        group(Piece.Joker, p3, Piece.Joker),
        group(p3, Piece.Joker, Piece.Joker)
      )
    )
  }

  test("1, 2, 3 and 1, 2, 3") {
    assertEquals(
      SequenceGroups(Bag(p1, p2, p3, p1, p2, p3)),
      Set(
        group(p1, p2, p3)
      )
    )
  }

  private def group(first: Piece, rest: Piece*) = Group.Run(NonEmptyList.of(first, rest*))

  private def piece(number: Int) = Piece.Fixed(Colour.Black, number)
}
