package rumikub.ai

import cats.data.NonEmptyList
import rumikub.model.*

class BestMovesFinderSuite extends munit.FunSuite {
  private val black1 = piece1(Colour.Black)
  private val black2 = blackPiece(2)
  private val black3 = blackPiece(3)
  private val black4 = blackPiece(4)
  private val black5 = blackPiece(5)
  private val blue1 = piece1(Colour.Blue)
  private val red1 = piece1(Colour.Red)
  private val yellow1 = piece1(Colour.Yellow)

  test("no available groups") {
    verify(
      Board.empty,
      Bag.empty
    )
  }

  test("one available group from original board") {
    verify(
      Board(List(numberGroup(black1, blue1, red1))),
      Bag(piece(Colour.Black, 2)),
      BoardAndPlayer(Board(List(numberGroup(black1, blue1, red1))), Bag(piece(Colour.Black, 2)))
    )
  }

  test("extend existing group") {
    verify(
      Board(List(numberGroup(black1, blue1, red1))),
      Bag(yellow1),
      BoardAndPlayer(
        Board(List(numberGroup(black1, blue1, red1, yellow1))),
        Bag.empty
      )
    )
  }

  test("ignore possibilities that don't use all required pieces") {
    verify(
      Board(List(numberGroup(black1, blue1, red1))), Bag(black2, black3),
      BoardAndPlayer(
        Board(List(numberGroup(black1, blue1, red1))),
        Bag(black2, black3)
      )
    )
  }

  test("add a new group") {
    verify(
      Board(List(numberGroup(black1, blue1, red1))), Bag(black3, black4, black5),
      BoardAndPlayer(
        Board(List(numberGroup(black1, blue1, red1), runGroup(black3, black4, black5))),
        Bag.empty
      )
    )
  }

  test("break an existing group to produce a new group") {
    verify(
      Board(List(numberGroup(black1, blue1, red1))), Bag(black2, black3, yellow1),
      BoardAndPlayer(
        Board(List(runGroup(black1, black2, black3), numberGroup(blue1, red1, yellow1))),
        Bag.empty
      )
    )
  }

  test("identical options") {
    verify(
      Board(List(numberGroup(black1, blue1, red1))), Bag(black1, blue1, red1),
      BoardAndPlayer(
        Board(List(numberGroup(black1, blue1, red1), numberGroup(black1, blue1, red1))),
        Bag.empty
      )
    )
  }

  private def verify(board: Board, player: Bag, expectedBestMoves: BoardAndPlayer*): Unit = {
    val initial = BoardAndPlayer(board, player)

    val found = BestMovesFinder(
      ValidGroupFinder(initial),
      initial
    )

    assertEquals(
      found.map(_.result.toList.toSet),
      NonEmptyList.fromList(expectedBestMoves.toList).map(BestMoves(_)).map(_.result.toList.toSet)
    )
  }

  private def numberGroup(piece: Piece, rest: Piece*) = Group.Number(NonEmptyList.of(piece, rest*))
  private def runGroup(piece: Piece, rest: Piece*) = Group.Run(NonEmptyList.of(piece, rest*))

  private def piece1(colour: Colour) = Piece.Fixed(colour, 1)
  private def blackPiece(number: Int) = Piece.Fixed(Colour.Black, number)
  private def piece(colour: Colour, number: Int): Piece = Piece.Fixed(colour, number)
}
