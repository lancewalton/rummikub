package rummikub.ai

import java.util.UUID
import rummikub.model.*
import rummikub.model.Piece.Fixed

class AISpec extends munit.FunSuite:

  private def red(n: Int): Fixed = Fixed(Colour.Red, n)
  private def player(rack: Bag, firstMove: Boolean): Player = Player(PlayerId(UUID.randomUUID().toString), "AI", rack, firstMove)

  test("finds a valid first move and returns the full board plus the reduced rack") {
    val result = AI(Board.empty, player(Bag(red(11), red(12), red(13)), firstMove = true))

    val move = result.getOrElse(fail("expected a move"))
    assert(move.board.groups.forall(_.isValid))
    assertEquals(move.board.pieces, Bag(red(11), red(12), red(13)))
    assert(move.player.isEmpty)
  }

  test("manipulates the board to play a rack tile") {
    val board  = Board(List(Group.Run(cats.data.NonEmptyList.of(red(3), red(4), red(5)))))
    val result = AI(board, player(Bag(red(6)), firstMove = false))

    val move = result.getOrElse(fail("expected a move"))
    assert(move.board.groups.forall(_.isValid))
    assertEquals(move.board.pieces, Bag(red(3), red(4), red(5), red(6)))
    assert(move.player.isEmpty)
  }
