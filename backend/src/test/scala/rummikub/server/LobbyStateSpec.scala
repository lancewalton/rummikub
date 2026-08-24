package rummikub.server

import rummikub.model.*
import rummikub.protocol.LobbyPlayer

class LobbyStateSpec extends munit.FunSuite:

  test("joining adds a human player") {
    val state = LobbyState.empty.join(PlayerId("p1"), "Alice")
    assertEquals(state.toLobbyPlayers, List(LobbyPlayer(PlayerId("p1"), "Alice", isAi = false)))
  }

  test("adding an AI adds an AI-flagged player") {
    val state = LobbyState.empty.addAi(PlayerId("ai1"), "Bot")
    assertEquals(state.toLobbyPlayers, List(LobbyPlayer(PlayerId("ai1"), "Bot", isAi = true)))
  }

  test("members are listed in join order") {
    val state = LobbyState.empty.join(PlayerId("p1"), "Alice").addAi(PlayerId("ai1"), "Bot")
    assertEquals(state.toLobbyPlayers.map(_.name), List("Alice", "Bot"))
  }

  test("starting with fewer than two players is not allowed") {
    assertEquals(LobbyState.empty.join(PlayerId("p1"), "Alice").startGame, None)
  }

  test("starting with two players creates a game containing both") {
    val state = LobbyState.empty.join(PlayerId("p1"), "Alice").join(PlayerId("p2"), "Bob")
    val game  = state.startGame.getOrElse(fail("expected a game"))
    assertEquals(game.players.keySet, Set(PlayerId("p1"), PlayerId("p2")))
  }
