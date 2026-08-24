package rummikub.ai.cpsat

import rummikub.model.*
import rummikub.model.Piece.{Fixed, Joker}

class MoveSolverSpec extends munit.FunSuite:

  private def red(n: Int): Fixed = Fixed(Colour.Red, n)
  private def rack(pieces: Piece*): Bag = Bag(pieces.toList)

  private def coloured(c: Colour, n: Int): Fixed = Fixed(c, n)
  private def runGroup(pieces: Piece*): Group = Group.Run(cats.data.NonEmptyList.fromListUnsafe(pieces.toList))
  private def setGroup(pieces: Piece*): Group = Group.Number(cats.data.NonEmptyList.fromListUnsafe(pieces.toList))

  test("plays an obvious first-move run from the rack") {
    val result = MoveSolver(Board.empty, rack(red(11), red(12), red(13)), firstMove = true)

    val move = result.getOrElse(fail("expected a move"))
    assert(move.board.groups.forall(_.isValid))
    assertEquals(move.board.pieces, Bag(red(11), red(12), red(13)))
    assert(move.player.isEmpty)
  }

  test("refuses a first move worth less than thirty") {
    val result = MoveSolver(Board.empty, rack(red(1), coloured(Colour.Blue, 1), coloured(Colour.Black, 1)), firstMove = true)

    assertEquals(result, None)
  }

  test("a joker fills a gap to complete a first move") {
    val result = MoveSolver(Board.empty, rack(red(5), Joker, red(7)), firstMove = true)

    val move = result.getOrElse(fail("expected a move"))
    assert(move.board.groups.forall(_.isValid))
    assertEquals(move.board.pieces, Bag(red(5), Joker, red(7)))
    assert(move.player.isEmpty)
  }

  test("extends a board run using a rack tile and conserves every board tile") {
    val board  = Board(List(runGroup(red(3), red(4), red(5))))
    val result = MoveSolver(board, rack(red(6)), firstMove = false)

    val move = result.getOrElse(fail("expected a move"))
    assert(move.board.groups.forall(_.isValid))
    assertEquals(move.board.pieces, Bag(red(3), red(4), red(5), red(6)))
    assert(move.player.isEmpty)
  }

  test("returns no move when nothing in the rack can be played") {
    val board  = Board(List(runGroup(red(3), red(4), red(5))))
    val result = MoveSolver(board, rack(coloured(Colour.Blue, 10)), firstMove = false)

    assertEquals(result, None)
  }
