package rummikub.server

import rummikub.model.*

class RematchSpec extends munit.FunSuite:

  test("a rematch re-deals a fresh game to the same players in the same order") {
    val original = Game.initial(List((PlayerId("p1"), "Alice"), (PlayerId("p2"), "Bob")))

    val next = Rematch(original)

    assertEquals(next.playerSequence, original.playerSequence)
    assertEquals(next.players(PlayerId("p1")).name, "Alice")
    assertEquals(next.players(PlayerId("p2")).name, "Bob")
    assert(next.players.values.forall(_.rack.piecesAsVector.size == 14), "every player starts with 14 tiles")
    assert(next.players.values.forall(_.firstMove), "first-move flag is reset")
    assert(next.board.isEmpty, "the board is cleared")
    assertEquals(next.currentPlayerId, original.playerSequence.head)
  }
